#include <stdio.h>
#include <stdlib.h>
#include <list>
#include <string>
#include <stdexcept>
#include <sstream>
#include <iostream>

using std::endl;
using std::cerr;

// Do not do "using std::list" because the name conflicts with list() function.
using std::string;
using std::exception;
using std::ostringstream;

#define LOG(x) puts(x)

/**
  Example things to parse:
  1. (a)
  2. (abc def (hij))

  Example to error out on:
  1. a
  2. (unbalanced
  3. unb)
  4. )un(
  5. (((un))

  */

/**
UPPERCASE are terminals, lowercase are non-terminals.
No escaping is allowed in this language. All strings are
raw and space separated. That means whitespace and comment
characters cannot appear in a string. Comments must appear after
whitespace, otherwise the semicolon comment character is treated
as part of the string.

file: lists
lists: list lists | EMPTY
list: '(' forms ')'
forms: form forms | EMPTY
form: STRING | list
EMPTY: 
STRING: [^ \n\r\t#]+
WS: [ \n\r\t]*
COMMENT : ';' [^\r\n]*
*/

#define DISABLE_COPY_AND_ASSIGN(name) \
    name(const name&) = delete; \
    name & operator=(const name&) = delete;

enum FormType {
    StringType,
    ListType
};

class Form;
typedef std::list<Form*> FormList;

class Form
{
public:
    DISABLE_COPY_AND_ASSIGN(Form);
    FormType const type_;
    FormList* forms_;
    string* string_;

    // StringType
    Form(char const* str, int len)
        : type_(StringType),
        forms_(NULL),
        string_(new string(str, len)) {}

    // ListType
    Form(FormList* forms)
        : type_(ListType),
        forms_(forms),
        string_(NULL) {}

    ~Form() {
        // Don't worry about cleanup. let the OS do that.
#if 0
        if (forms_) {
            for(Form* f : forms_) {
                delete f;
            }
        }
        delete forms_;
        delete string_;
#endif
    }
};

enum Symbol {
    SymbolNone,
    SymbolLeftParen,
    SymbolRightParen,
    SymbolString,
};

#define MAX_STRING 10000

class Parser
{
    DISABLE_COPY_AND_ASSIGN(Parser);

    FILE* fp_;
    Form* root_;
    Symbol symbol_;
    int line_; // current line being parsed.
    char string_[MAX_STRING];
    int stringlen_;

    FormList* lists();
    Form* list();
    FormList* forms();
    Form* form();
    void getsym();
    void getstringsym(int first_char);
    bool expect(Symbol);
    bool accept(Symbol);

public:
    Parser(FILE* fp);
    ~Parser();
    FormList* parse();
};

Parser::Parser(FILE* fp)
    : fp_(fp),
    root_(NULL),
    stringlen_(0)
{
}

Parser::~Parser()
{
    if (fp_) fclose(fp_);
}

FormList* Parser::parse()
{
    line_ = 1;
    FormList* result = NULL;
    try {
        LOG("parse");
        getsym();
        result = lists();
    } catch (std::exception & e) {
        cerr << "Exception occurred on line " << line_ << ": " << e.what() << endl;
    } catch(...) {
        cerr << "Unknown exception on line " << line_ << "." << endl;
    }
    return result;
}

#define CASE_WS \
    case '\n': \
    case '\r': \
    case '\t': \
    case ' '

void Parser::getsym()
{
    LOG("getsym");
    int c;
    Symbol sym = SymbolNone;

    while(sym == SymbolNone && (c = fgetc(fp_)) != EOF) {
        switch(c) {
        CASE_WS:
            if (c == '\n' || c == '\r') {
                ++line_;
            }
            // skip whitespace
            break;
        case '(':
            sym = SymbolLeftParen;
            break;
        case ')':
            sym = SymbolRightParen;
            break;
        case ';':
            // eat comment
            while(c != EOF && c != '\n' && c != '\r') {
                c = fgetc(fp_);
            }
            ungetc(c, fp_);
            break;
        default:
            sym = SymbolString;
            getstringsym(c);
            break;
        }
    }

    symbol_ = sym;
}

void Parser::getstringsym(int c)
{
    LOG("getstringsym");
    stringlen_ = 0;
    bool recognized = false;

    do {
        switch(c) {
        CASE_WS:
        case '(':
        case ')':
            ungetc(c, fp_);
            recognized = true;
            break;
        default:
            // note: no check of max string length here
            string_[stringlen_++] = c;
            break;
        }
        c = fgetc(fp_);
    } while(!recognized && c != EOF && stringlen_ < MAX_STRING - 1);

    if (stringlen_ == MAX_STRING - 1) {
        fprintf(stderr, "String exceeded max string length of %d\n", MAX_STRING);
        exit(1);

        ostringstream oss;
        oss << "String exceeded max string length of " << MAX_STRING;
        throw std::runtime_error(oss.str());
    }
}


bool Parser::expect(Symbol s)
{
    LOG("expect");
    if (accept(s)) return true;
    fprintf(stderr, "expect: unexpected symbol\n");
    return false;
}

bool Parser::accept(Symbol s)
{
    LOG("accept");
    if (symbol_ == s) {
        getsym();
        return true;
    }
    return false;
}

FormList* Parser::lists()
{
    LOG("lists");
    FormList* result = new FormList();
    while(symbol_ == SymbolLeftParen) {
        result->push_back(list());
    }
    return result;
}

Form* Parser::list()
{
    LOG("list");
    expect(SymbolLeftParen);
    auto formList = forms();
    expect(SymbolRightParen);
    return new Form(formList);
}


FormList* Parser::forms()
{
    LOG("forms");
    FormList* result = new FormList();
    Form* f;
    while((f = form()) != NULL) {
        result->push_back(f);
    }
    return result;
}

Form* Parser::form()
{
    LOG("form");
    Form* result = NULL;
    if (symbol_ == SymbolString) {
        result = new Form(string_, stringlen_);
        getsym();
    } else if (symbol_ == SymbolLeftParen) {
        result = list();
    }
    return result;
}

int main(int argc, char** argv)
{
    Parser parser(stdin);
    FormList* list = parser.parse();
    if (list == NULL) {
        exit(1);
    }

    return 0;
}

