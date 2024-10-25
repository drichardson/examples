/-
https://lean-lang.org/functional_programming_in_lean/type-classes.html
-/
def main : IO Unit :=
  IO.println s!"Hello, world!"

/-
Positive Numbers
https://lean-lang.org/functional_programming_in_lean/type-classes/pos.html

One way to represent positive numbers is very similar to Nat, except with one as the base case instead of zero:
-/

inductive Pos : Type where
  | one : Pos
  | succ : Pos → Pos
deriving Repr

/-
This datatype represents exactly the intended set of values, but it is not very convenient to use. For example, numeric literals are rejected:
-/
def seven : Pos := 7


-- instead, have to use

def seven_v2 : Pos :=
  Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ (Pos.succ Pos.one)))))

#eval seven_v2


/-
One way to overload addition is to define a type class named Plus, with an addition method named plus.
-/
class Plus (α : Type) where
  plus : α → α → α

/-
To overload plus for a particular type, write an instance:
-/
instance : Plus Nat where
  plus := Nat.add

#eval Plus.plus 5 3

def Pos.plus : Pos → Pos → Pos
  | Pos.one, k => Pos.succ k
  | Pos.succ n, k => Pos.succ (n.plus k)

instance : Plus Pos where
  plus := Pos.plus

#eval Plus.plus seven_v2 seven_v2

/-
Exercise: An alternative way to represent a positive number is as the successor of some Nat. Replace the definition of Pos with a structure whose constructor is named succ that contains a Nat.

Define instances of Add, Mul, ToString, and OfNat that allow this version of Pos_v2 to be used conveniently.
-/

structure Pos_v2 where
  succ ::
  pred : Nat
deriving Repr

def p1 := Pos_v2.succ 2
def p2 := Pos_v2.succ 3

#eval p1
#eval p2

#eval p1 + p2

section
variable (x y : Pos_v2)

def Point_v2.add x y := Pos_v2.succ (x.pred + y.pred)
end

instance : Add Pos_v2 where
  --add := fun (x : Pos_v2) (y : Pos_v2) => Pos_v2.succ (x.pred + y.pred)
  add := fun (x : Pos_v2) (y : Pos_v2) => Pos_v2.succ (x.pred + y.pred)

#eval p1 + p2
