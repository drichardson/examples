# relative import
from ..p1 import mod2

def foo():
    print(__name__)
    print('calling p1.mod2.foo()')
    mod2.foo()
