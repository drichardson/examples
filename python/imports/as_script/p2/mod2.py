# relative import
#import ..p1.mod2
#import p1.mod2
from p1 import mod2

def foo():
    print(__name__)
    print('calling mod2.foo()')
    mod2.foo()
