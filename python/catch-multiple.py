class ExceptionA(Exception):
    pass

class ExceptionB(Exception):
    pass

class ExceptionC(Exception):
    pass
        

def raisesA():
    raise ExceptionA("ExceptionA was raised in raisesA")


def foo():
    for e in [ExceptionA, ExceptionB, ExceptionC]:
        try:
            raise e("an exception")
        except (ExceptionA, ExceptionB):
            print("Caught exception A or B")
        except Exception:
            print("Caught Other")


foo()
