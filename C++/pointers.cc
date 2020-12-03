#include <iostream>
#include <memory>

using std::cout;

class A
{
public:
    int i = 0;
    ~A() {
        cout << "~A\n";
    }
};

std::ostream& operator<<(std::ostream& os, const A& a)
{
    os << "A value " << a.i;
    return os;
}

int main()
{
    {
        auto iptr = std::make_unique<int>(1234);
        cout << *iptr << "\n";
    }

    cout << "Section 1\n";
    {
        std::unique_ptr<A> aptr2;
        cout << "entering scope\n";
        {
            cout << "in scope\n";
            auto aptr = std::make_unique<A>();
            cout << "(pre move) aptr = " << aptr.get() << "\n";
            aptr2 = std::move(aptr);
            cout << "(post move) aptr = " << aptr.get() << "\n";
            cout << "leaving scope\n";
        }
        cout << "out of scope\n";
    }

    cout << "Section 2\n";
    {
        std::shared_ptr<A> aptr2;
        cout << "entering scope\n";
        {
            cout << "in scope\n";
            auto aptr = std::make_shared<A>();
            aptr2 = aptr;
            cout << "leaving scope\n";
        }
        cout << "out of scope\n";
    }

    return 0;
}

