import logging


root = logging.getLogger()
a = logging.getLogger("a")
ab = logging.getLogger("a.b")
b = logging.getLogger("b")

ab.setLevel(logging.INFO)

all_loggers = [logging, root, a, ab, b]

def info(message):
    for l in all_loggers:
        l.info(message)

def warn(message):
    for l in all_loggers:
        l.warning(message)

info("info 1")
warn("warn 1")

# non keyward arguments
root.error("no args")
root.error("one arg %s", 123)
root.error("two args %s %d", 123, 234)
root.error("three args %s %d %f", "strarg", 123.5, 3.14)
#root.error("no args %(someval)s", {someval : "123"})
#root.error("no args %(someval)s", {someval:"123"})


root.error("exc_info log", exc_info=True)
root.error("stack_info log", stack_info=True)
root.error("stacklevel log", stack_info=True, stacklevel=0)

def foo():
    bar()

def bar():
    root.error("stacklevel log from bar", stack_info=True, stacklevel=3)

foo()

root.error("extra log", extra={"key1" : "val1", "key2" : 123})

root.log(logging.ERROR, "log with ERROR level")

root.error("root has handlers: %s", root.hasHandlers())
root.error("a has handlers: %s", a.hasHandlers())
root.error("ab has handlers: %s", ab.hasHandlers())

handler1 = logging.StreamHandler()
handler1.setLevel(logging.INFO)
format1 = logging.Formatter('%s(name)s - %(levelname)s - %(message)s')
handler1.setFormatter(format1)
#root.handlers[0].setFormatter(format1)

root.info("after format1 1")
root.addHandler(handler1)
root.info("after format1 2")
