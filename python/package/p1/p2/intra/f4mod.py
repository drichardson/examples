# Intra-package Reference example

# Relative package imports

from ... import f2
from .. import p3
from ..p3 import f3
from . import f5mod

def f4():
    print("f4: intra func, calling ")
    f2.func()
    p3.f3.func()
    f3.func()
    f5mod.f5()
    print("f4: intra func, end")
