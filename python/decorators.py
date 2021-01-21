#!/usr/bin/env python3

# PEP 318 - Decorators for Functions and Methods
# https://www.python.org/dev/peps/pep-0318/

# Python Reference: 8.6 Function definitions
# https://docs.python.org/3/reference/compound_stmts.html#function

# a custom decorator that passes val to f for an instance function.
def my_decorator(f):
    print('my_decorator: decorating {0}'.format(f))
    def wrapper(self):
        print('wrapper calling f')
        f(self, 555)
        print('wrapper f returned')
    return wrapper

class A:

    def __init__(self, v=0):
        self.my_var = v

    # A non-decorated instance method.
    def my_instance_method(self, arg1):
        print('my_instance_method: self={0}, arg1={1}, self.myvar={2}'.format(self, arg1, self.my_var))

    # PEP 318 custom decorator
    @my_decorator
    def my_instance_method2(self, arg1):
        print('my_instance_method2: self={0}, arg1={1}, self.myvar={2}'.format(self, arg1, self.my_var))


    @classmethod
    def my_class_method(cls, arg1):
        print('my_class_method: cls={0}, arg1={1}'.format(cls, arg1))

    # Pre PEP 318 function transformations
    def my_class_method2(cls, arg1):
        print('my_class_method2: cls={0}, arg1={1}'.format(cls, arg1))
    my_class_method2 = classmethod(my_class_method2)

    @staticmethod
    def my_static_method(arg1):
        print('my_static_method: arg1={0}'.format(arg1))

    def my_static_method2(arg1):
        print('my_static_method2: arg1={0}'.format(arg1))
    my_static_method2 = staticmethod(my_static_method2)


    # This works as a static method for a class, but doesn't work
    # with instance calling notation since self is passed in for the
    # first argument..
    def something_weird(arg1):
        print('something_weird: arg1={0}'.format(arg1))



print("decorators")
A.my_class_method('a string')
A.my_class_method2('another string')
A.my_instance_method(A(1), 'val 1')
A.my_instance_method2(A(2))
A.my_static_method('val 1')
A.my_static_method2('val 1')
A.something_weird('val 1')

a = A(10)
a.my_class_method('hello') # still uses class, not instance
a.my_class_method2('hello 2') # still uses class, not instance
a.my_instance_method('val 2')
a.my_instance_method2()
a.my_static_method('val 2')
a.my_static_method2('val 2')
# a.something_weird('val 2') # error: takes 1 positional argument but 2 were given
