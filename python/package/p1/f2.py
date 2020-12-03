def func():
    print("p1/f2.func called")

def p2():
    """p2 intentionally conflicts with the p2 module in this package."""
    print("p1/p2 called")

