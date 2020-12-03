#include <iostream>
#include <string>
#include <string_view>

using std::cout;

int main()
{
    std::string defaultvalue;
    std::string fromcharptr("testing 1");
    std::string fromstring(fromcharptr);

    using namespace std::string_literals;
    std::string fromliteral = "literal"s;

    cout << "defaultvalue: " << defaultvalue << "\n";
    cout << "fromcharptr: " << fromcharptr << "\n";
    cout << "fromstring: " << fromstring << "\n";
    cout << "fromliteral: " << fromliteral << "\n";

    std::string_view sv_default;
    std::string_view entire(fromcharptr);
    std::string_view prefix = entire.substr(0, 5);
    std::string_view middle = entire.substr(3, 3);
    std::string_view end = entire.substr(3);

    cout << "sv_default: " << sv_default << "\n";
    cout << "entire: " << entire << "\n";
    cout << "prefix: " << prefix << "\n";
    cout << "middle: " << middle << "\n";
    cout << "end: " << end << "\n";

    return 0;
}


