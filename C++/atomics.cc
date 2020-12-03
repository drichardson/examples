#include <atomic>
#include <iostream>

using std::cout;
using std::endl;

int main() {

    std::atomic<int> val{0};

    val.store(1);
    int i = val.load();
    bool ok = val.compare_exchange_strong(i, 5);

    cout << "ok is " << ok << endl;

    int ten = 10;
    ok = val.compare_exchange_strong(ten, 5);
    cout << "ok is " << ok << endl;
}

