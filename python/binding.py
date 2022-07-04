class A:
    def foo(self, a):
        print(f"foo({id(self)}, {a}) called")


a = A()
a.foo(1)
a.foo(2)

b = A()
b.foo(3)

x = a.foo
x(5)

x = b.foo
x(6)

