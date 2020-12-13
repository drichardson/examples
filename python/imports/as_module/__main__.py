# absolute
import as_module.p1.mod1
from as_module.p1 import mod2

# relative
from .p2 import mod1 as p2mod1, mod2 as p2mod2

print('as_module/__main__.py __name__ is ' + __name__)

as_module.p1.mod1.foo()
mod2.foo()
p2mod1.foo()
p2mod2.foo()
