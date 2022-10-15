class Animal:
    def __init__(self, name, sound):
        self.name = name
        self.sound = sound

    def talk(self):
        print("%s: %s" % (self.name, self.sound))

    @classmethod
    def my_class_method(class_arg, arg1=123):
        print("my_class_method", class_arg, arg1)

    @staticmethod
    def my_static_method(arg1=123):
        print("my_static_method", arg1)

    def naked_method(arg1=None):
        print("naked_method: first arg is self if called on object, or None if called on class", arg1)

class Tiger(Animal):
    def __init__(self):
        Animal.__init__(self, "Tiger", "Roar!!!")

class Horse(Animal):
    def __init__(self):
        Animal.__init__(self, "Horse", "Whinny.")


if __name__ == "__main__":
    t = Tiger()
    t.talk()

    h = Horse()
    h.talk()

    Tiger.my_class_method()
    t.my_class_method()

    Tiger.my_static_method()
    t.my_static_method()

    Tiger.naked_method()
    t.naked_method()


