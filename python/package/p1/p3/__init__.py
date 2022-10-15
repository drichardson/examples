print(__name__)
print("HI")

def foo():
    print("foo called")

def bar():
    print("bar called")

__all__ = [
        "foo",
        "bar"
]
