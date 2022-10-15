class C:
    def __init__(self):
        self._x = None

    def _getx(self):
        print("getting x")
        return self._x

    def _setx(self, value):
        print(f"setting x to {value}")
        self._x = value

    def _delx(self):
        print("deleting x")
        del self._x

    x = property(_getx, _setx, _delx, "I'm the 'x' property.")


help(C)
c = C()

print(f"{c.x}")
c.x = 123
print(f"{c.x}")
del c.x
#print(f"{c.x}")


class Parrot:
    def __init__(self):
        self._voltage = 100000

    @property
    def voltage(self):
        """Get the current voltage."""
        return self._voltage


help(Parrot)
p = Parrot()
print(f"parrot voltage {p.voltage}")
