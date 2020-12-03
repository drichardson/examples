#include <iostream>

using namespace std;

struct A {
    ~A() { cout << "A "; }
};

struct AVirt {
    virtual ~AVirt() { cout << "AVirt "; }
};

struct B_A : public A {
    ~B_A() { cout << "B_A "; }
};

struct B_AVirt : public AVirt {
    ~B_AVirt() { cout << "B_AVirt "; }
};

struct BVirt_A : public A {
    virtual ~BVirt_A() { cout << "BVirt_A "; }
};

struct C_BVirt : public BVirt_A {
    ~C_BVirt() { cout << "C_BVirt "; }
};

void deleteA(A* a) {
    cout << "deleteA: ";
    delete a;
    cout << '\n';
}

void deleteB_A(B_A* b) {
    cout << "deleteB_A: ";
    delete b;
    cout << '\n';
}

void deleteAVirt(AVirt* aVirt) {
    cout << "deleteAVirt: ";
    delete aVirt;
    cout << '\n';
}

void deleteBVirt_A(BVirt_A* b) {
    cout << "deleteBVirt_A: ";
    delete b;
    cout << '\n';
}

int main() {
    deleteA(new A);
    deleteA(new B_A);
    deleteA(new BVirt_A);
    deleteB_A(new B_A);
    deleteAVirt(new AVirt);
    deleteAVirt(new B_AVirt);
    deleteBVirt_A(new BVirt_A);
    deleteA(new C_BVirt);
    deleteBVirt_A(new C_BVirt);
}

