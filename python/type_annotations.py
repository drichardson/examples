#!/usr/bin/env python3

#
# To check type annotations in this file with a static checker, you can run:
# python3 -m pip install mypy
# mypy type_annotations.py
#
# For more information, see: https://mypy.readthedocs.io
#

def print_annotations(msg):
    print(f"{msg} annotations: {__annotations__}")

print_annotations("Before my_var")

my_var: int


print_annotations("After my_var")

my_var = 1

print_annotations(f"After my_var=1 {my_var}")

my_var: str

print_annotations(f"After my_var change type {my_var}")

my_var = 'two'

print_annotations(f"After my_var='two' {my_var}")

class C1:
    x: int=1


class C2:
    x=1

    def __init__(self):
        print("C2 constructor called")

print_annotations("After C1 declared")

c1 = C1()
c2 = C2()

print_annotations("After c1 instanciated")

print(f"C1 annotations: {C1.__annotations__}")
# print(f"C2 annotations: {C2.__annotations__}") # Raises error because C2 has no annotations

print(f"c1 annotations: {c1.__annotations__}")
# print(f"c2 annotations: {c2.__annotations__}") # Raises error because C2 (and c2) has no annotations



c1.__annotations__.clear()
print(f"c1 annotations after clear(): {c1.__annotations__}")


