#include "list.hpp"
#include <iostream>

typedef containers::singly_linked_list<unsigned> ulist;

void setup_list(ulist & l) {
    for(unsigned x = 0; x < 5; ++x) {
        l.insert_front(x);
    }
}

struct myPOD {
    int x;
    int y;
};

int main() {
    using std::cout;

    {
        cout << "list fundamentals: ";
        ulist list;
        setup_list(list);
        while(!list.empty()) {
            ulist const & lc = list;
            cout << lc.front() << ' ';
            list.pop_front();
        }
        cout << '\n';
    }

    {
        cout << "list iterator: ";
        ulist list;
        setup_list(list);
        for(ulist::iterator itr = list.get_iterator();
                !itr.end(); itr.moveNext()) {
            cout << itr.get() << ' ';
        }
        cout << '\n';
    }
    
    {
        cout << "const list iterator: ";
        ulist list;
        setup_list(list);
        for(ulist::const_iterator itr = list.get_const_iterator();
                !itr.end(); itr.moveNext()) {
            cout << itr.get() << ' ';
        }
        cout << '\n';
    }

    {
        cout << "POD: ";
        containers::singly_linked_list<myPOD> list;
        for(int x = 1; x <= 5; ++x) {
            myPOD v = {x, x*x};
            list.insert_front(v);
        }
        for(containers::singly_linked_list<myPOD>::const_iterator i = list.get_const_iterator();
                !i.end(); i.moveNext()) {
            cout << "(x: " << i.get().x << " y=" << i.get().y << ") ";
        }
        cout << '\n';
    }
}

