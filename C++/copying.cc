#include <iostream>
#include <string>
#include <vector>

using std::cout;
using std::string;

class CopyCountLogger
{
    int begin;
    static int copyCounter;
public:
    static void Increment() { copyCounter++; }
    CopyCountLogger() : begin(copyCounter) {}
    ~CopyCountLogger() { cout << copyCounter - begin << " copies\n"; }
};

int CopyCountLogger::copyCounter = 0;

class A
{
public:
    string s;

    A() = delete;

    // A(const A& other) = delete;
    A(const A& other) : A(other.s + "COPY") { CopyCountLogger::Increment(); }

    A& operator=(const A&) = delete;

    A(string s_) : s(s_) { cout << "A(" << s_ << ")\n"; }
    ~A() { cout << "~A(" << s << ")\n"; }
};

A ReturnRValue(string s)
{
    return A(s);
}

A ReturnLValue(string s)
{
    A a(s);
    cout << "ReturnLValue: " << a.s << "\n";
    return a;
}

static void LoopTest(int count, bool use_emplace, bool reserve_space)
{
    std::vector<A> v;

    // Reserving space avoids copy that occur due to vector resize.
    if (reserve_space)
    {
        v.reserve(count);
    }

    cout << "**********************************\n";
    cout << "LOOP TEST: count=" << count << " use_emplace=" << use_emplace << " reserve_space=" << reserve_space << "\n";
    cout << "**********************************\n";
    {
        cout << "BEGIN LOOP FILL\n";
        CopyCountLogger logger;
        for(int i = 0; i < count; ++i)
        {
            cout << "Loop: " << i << "\n";
            if (use_emplace)
            {
                v.emplace_back("Item: " + std::to_string(i));
            }
            else
            {
                v.push_back("Item: " + std::to_string(i));
            }
        }
        cout << "END LOOP FILL\n";
    }

    cout << "ITEMS:\n";
    for(const A& a : v)
    {
        cout << "  " << a.s << "\n";
    }

    cout << "**********************************\n";
    cout << "END LOOP TEST: count=" << count << " use_emplace=" << use_emplace << " reserve_space=" << reserve_space << "\n";
    cout << "**********************************\n";
}

int main()
{
    cout << "1\n";
    {
        CopyCountLogger();
        A("a1");
    }
    cout << "2\n";
    {
        CopyCountLogger();
        A a = ReturnRValue("a2");
        ReturnRValue("a3");
    }
    cout << "3\n";
    {
        CopyCountLogger();
        A a = ReturnLValue("a4");
        cout << "a is " << a.s << "\n";
    }
    cout << "4\n";
    
    LoopTest(std::stoi("12"), false, false);
    LoopTest(std::stoi("12"), false, true);
    LoopTest(std::stoi("12"), true, false);
    LoopTest(std::stoi("12"), true, true);

    return 0;
}

