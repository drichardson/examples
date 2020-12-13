# absolute import
import as_module.p1.mod1

def foo():
    print(__name__)
    print('calling as_module.p1.mod1.foo()')
    as_module.p1.mod1.foo()
