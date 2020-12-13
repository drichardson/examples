import p1.mod1

def foo():
    print(__name__)
    print('p1.mod1.foo()')
    p1.mod1.foo()
