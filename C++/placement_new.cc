#include <iostream>
#include <string>
#include <type_traits>

using namespace std;

class A {
    int _i;
    char _c;
public:
    A(int i, char c) : _i(i), _c(c) {
        cout << this << "=A(" << _i << "," << _c << ")\n";
    }

    ~A() {
        cout << this << "=~A(" << _i << "," << _c << ")\n";
    }
};

struct NoCopy
{
    NoCopy() {}
    NoCopy(const NoCopy&) = delete;
    NoCopy& operator=(const NoCopy&) = delete;
};

struct MyItem : NoCopy
{
    MyItem() = delete;
    MyItem(int id, const char* str) : id(id), str(str) { cout << "MyItem(" << id << ")\n"; }
    ~MyItem() { cout << "~MyItem(" << id << ")\n"; }

    int id;
    std::string str;
};

class MyStack : NoCopy
{
    static constexpr int MaxItems = 10;
    std::aligned_storage<sizeof(MyItem), alignof(MyItem)>::type data[MaxItems];
    std::size_t count;

    MyItem* get(int index) { return reinterpret_cast<MyItem*>(data) + index; }
    MyItem const* get(int index) const { return reinterpret_cast<MyItem const*>(data) + index; }
public:

    MyStack() : count(0) {}

    ~MyStack()
    {
        while(count > 0)
        {
            pop();
        }
    }

    void push(int id, const char* str)
    {
        if (count + 1 >= MaxItems)
        {
            cout << "ERROR: NO MORE SPACE\n";
            return;
        }

        new(get(count)) MyItem(id, str);
        count++;
    }

    const MyItem& top() const
    {
        return *get(count-1);
    }

    void pop()
    {
        count--;
        get(count)->~MyItem();
    }

    std::size_t size() const { return count; }
};

int main() {

    // basics
    cout << "basics\n\n";
    {
        A a{1, 'a'};
        unsigned char* blob[10000];
        int next {0};
        A* p = new(blob+next) A{2, 'b'};
        next += sizeof(A);
        A* p2 = new(blob+next) A{3, 'c'};
        p->~A();
        p2->~A();
    }


    // my own stack built using placement new and explicit destructor calling.
    cout << "\nmy stack\n\n";
    {
        MyStack s;
        s.push(1, "one");
        s.push(2, "two");
        s.push(3, "three");

        while(s.size() > 0)
        {
            cout << "top is: " << s.top().id << ", " << s.top().str << "\n";
            s.pop();
        }
    }
}

