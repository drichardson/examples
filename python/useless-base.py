class MyClass:
    def foo(self):
        print("MyClass: {}".format(self))


class MyClass2(object):
    def foo(self):
        print("MyClass2: {}".format(self))


c1 = MyClass()
c2 = MyClass2()

c1.foo()
c2.foo()
