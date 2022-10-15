class ExceptionA(Exception):
    pass

class ExceptionB(Exception):
    pass
        

def raisesA():
    raise ExceptionA("ExceptionA was raised in raisesA")


def foo():
    try:
        raisesA()
    except Exception as exc:
        print("Caught exception in foo")
        raise ExceptionB("BBB")


foo()
