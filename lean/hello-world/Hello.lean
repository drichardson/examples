/-
When Lean is invoked with the --run option, it invokes the program's main definition. In programs that do not take command-line arguments, main should have type IO Unit. This means that main is not a function, because there are no arrows (→) in its type. Instead of being a function that has side effects, main consists of a description of effects to be carried out.

As discussed in the preceding chapter, Unit is the simplest inductive type. It has a single constructor called unit that takes no arguments. Languages in the C tradition have a notion of a void function that does not return any value at all. In Lean, all functions take an argument and return a value, and the lack of interesting arguments or return values can be signaled by using the Unit type instead. If Bool represents a single bit of information, Unit represents zero bits of information.
-/

def main : IO Unit := IO.println "Hello, world!"
