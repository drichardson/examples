class MyClass:
    foo = 'hi'

    def __init__(self):
        print(f"HI is: {self.foo}")
        self.foo = None
        print(f"HI is: {self.foo}")
        self.foo = '1234'

    def foo(self):
        print("MyClass: {}".format(self.hi))

    foo = 'lala'


c1 = MyClass()
print(f"foo is {c1.foo}")
c1.foo()
