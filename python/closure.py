def foo():
    print("foo")

    for i in range(0, 10):
        print("i: {}".format(i))

        loop_var = {'val': i}
        f = lambda x: print(loop_var.get(x))
        f('val')
foo()
