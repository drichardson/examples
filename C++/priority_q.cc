#include <iostream>
#include <queue>
#include <cstring>

class MyThing
{
    char const* _s;
public:
    MyThing(char const* s) : _s{s} {}

    // ordered by length of string
    bool operator<(MyThing const & other) const {
        return strlen(_s) < strlen(other._s);
    }

    char const* s() const { return _s; }
};

std::ostream& operator<<(std::ostream& out, MyThing const & thing)
{
    out << thing.s();
    return out;
}

template<typename Q>
void dump(Q & q)
{
    std::cout << "q (" << q.size() << " elements) : ";
    while(!q.empty()) {
        std::cout << q.top() << ", ";
        q.pop();
    }
    std::cout << std::endl;
}

int main()
{
    std::priority_queue<int> q;
    dump(q);

    q.push(1); q.push(2);
    dump(q);

    q.push(3); q.push(1);
    dump(q);

    std::priority_queue<MyThing> q2;
    q2.push(MyThing{"hi"});
    dump(q2);

    q2.push("aa"); q2.push("aaa"); q2.push("a"); q2.push("A"); q2.push("AAAAA");
    dump(q2);
}

