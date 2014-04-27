#include <stdio.h>
#include <stdlib.h>
#include <list>
#include <string>
#include <stdexcept>
#include <sstream>

using std::list;
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
form: literal | list
list: '(' form* ')'
literal: STRING
STRING: [^ \t\n\r]+
WS: [ \n\r\t]*
COMMENT : [#].*
*/

#define DISABLE_COPY_AND_ASSIGN(name) \
    name(const name&) = delete; \
    name & operator=(const name&) = delete;

enum FormType {
    StringType,
    ListType
};

class Form
{
public:
    DISABLE_COPY_AND_ASSIGN(Form);
    FormType const type_;
    list<Form*>* forms_;
    string* string_;

    // StringType
    Form(char const* str, int len)
        : type_(StringType),
        forms_(NULL),
        string_(new string(str, len)) {}

    // ListType
    Form(list<Form*>* forms)
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
    char string_[MAX_STRING];
    int stringlen_;

    Form* listForm();
    Form* form();
    list<Form*> *forms();
    void getsym();
    void getstringsym(int first_char);
    bool expect(Symbol);
    bool accept(Symbol);

public:
    Parser(FILE* fp);
    ~Parser();
    Form* parse();
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

Form* Parser::parse()
{
    Form* result = NULL;
    try {
        LOG("parse");
        getsym();
        result = listForm();
    } catch (...) {

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
            // skip whitespace
            break;
        case '(':
            sym = SymbolLeftParen;
            break;
        case ')':
            sym = SymbolRightParen;
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

Form* Parser::listForm()
{
    LOG("listForm");
    expect(SymbolLeftParen);
    auto result = forms();
    expect(SymbolRightParen);
    return new Form(result);
}


list<Form*>* Parser::forms()
{
    accept(SymbolRightParen);

    LOG("forms");

    // TODO
    abort();
    return NULL;
}

int main(int argc, char** argv)
{
    Parser parser(stdin);
    parser.parse();
    return 0;
}

