# https://docs.python.org/3/glossary.html#term-descriptor
# Any object which defines the methods __get__(), __set__(), or __delete__().

# https://docs.python.org/3/howto/descriptor.html#descriptor-howto-guide

# Example 1: lookup a constant.
class Ten:
    def __get__(self, instance, owner):
        return 10


# To use the descriptor, it must be stored as a class variable in another class:
class A:
    x = 5
    y = Ten()


a = A()
print(f"a.x={a.x} and a.y={a.y}")


# Example 2: dynamic lookups
class Testing(UserObject):
    def __init__(self):
        
        UserObject.__init__(self)

        

class MyClass:
    def __init__(self):
        
        .__init__(self)

        pass

class DirectorySize(object):
    def __init__(self):
        
        pass
