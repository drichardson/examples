#include "vector.hpp"
#include <iostream>


namespace {

void setup_vector(containers::vector<unsigned> & v)
{
    for(unsigned i = 0; i < v.size(); ++i) {
        v.get(i) = i;
    }
}

class A {
    void copy(A const & rhs)
    {
        x = rhs.x;
        y = rhs.y;
        delete[] buf;
        buf = new char[10];
        buf[0] = 'C';
        buf[1] = 0;
    }

public:
    int x;
    int y;
    char *buf;
    char xyz[4];
    static int balance;

    A(A const & rhs) : buf(NULL)
    {
        balance++;
        //std::cout << "A copy ctor=" << this << '\n';
        copy(rhs);
    }

    A(int x, int y) {
        balance++;
        this->x = x;
        this->y = y;
        buf = new char[10];
        buf[0] = 'H';
        buf[1] = 0;
        //std::cout << "A={" << this << "\n";
    }


    A & operator=(A const & rhs)
    {
        copy(rhs);
        return *this;
    }

    ~A() {
        balance--;
        //std::cout << "~A=" << this << "buf=" << static_cast<void*>(buf) << "=" << buf << "\n";
        delete[] buf;
    }
};

int A::balance = 0;

}

int main()
{
    using std::cout;
    typedef containers::vector<unsigned> uvec;

    {
        cout << "basic: ";
        uvec v(5, 10);
        for(size_t i = 0; i < v.size(); ++i) {
            cout << '(' << i << '=' << v.get(i) << ") ";
        }
        cout << '\n';
    }

    {
        cout << "pod: ";
        A p(5,6);

        containers::vector<A> v(5, p);
        for(size_t i = 0; i < v.size(); ++i) {
            cout << '(' << i << '=' << v.get(i).x << ',' << v.get(i).y << ") ";
        }
        cout << '\n';
    }

    {
        cout << "iterator: ";
        uvec v(5, 0);
        setup_vector(v);
        for(uvec::iterator i = v.get_iterator(); !i.end(); i.moveNext()) {
            cout << i.get() << ' ';
        }
        cout << '\n';
    }

    {
        cout << "resize: ";
        uvec v(0,0);
        for(size_t len = 1; len < 100; ++len) {
            v.resize(len);
            cout << v.size() << ' ';
        }
        cout << '\n';
    }

    {
        cout << "resize A: ";
        A p(1,2);
        containers::vector<A> v(0,p);
        for(size_t len = 1; len < 100; ++len) {
            v.resize(len, A(len,2*len));
            cout << v.size() << ' ';
        }
        cout << '\n';
        for(size_t len = 100; len >= 1; --len) {
            v.resize(len, A(len, -2*len));
            cout << v.size() << ' ';
        }
        cout << '\n';
    }

    {
        cout << "set A: ";
        A p(1,2);
        containers::vector<A> v(10,p);
        v.set(0, A(2,2));
        v.set(1, A(3,3));
        v.set(2, A(4,4));
        v.get(3) = A(5,5);
        for(size_t i = 0; i < v.size(); ++i) {
            cout << "v[" << i << "]={" << v.get(i).x << ',' << v.get(i).y << "} ";
        }
        cout << '\n';
    }

    cout << "balance is: " << A::balance << " (should be 0)\n";
}
