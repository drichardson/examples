/-
Dependent type theory is a powerful and expressive language, allowing you to express complex mathematical assertions, write complex hardware and software specifications, and reason about both of these in a natural and uniform way. Lean is based on a version of dependent type theory known as the Calculus of Constructions, with a countable hierarchy of non-cumulative universes and inductive types. By the end of this chapter, you will understand much of what this means.
-/

-- Simple Type Theory

/- Define some constants. -/

def m : Nat := 1       -- m is a natural number
def n : Nat := 0
def b1 : Bool := true  -- b1 is a Boolean
def b2 : Bool := false

/- Check their types. -/

#check m            -- output: Nat
#check n
#check n + 0        -- Nat
#check m * (n + 0)  -- Nat
#check b1           -- Bool
#check b1 && b2     -- "&&" is the Boolean and
#check b1 || b2     -- Boolean or
#check true         -- Boolean "true"

/- Evaluate -/

#eval 5 * 4         -- 20
#eval m + 2         -- 3
#eval b1 && b2      -- false


#check Nat → Nat      -- type the arrow as "\to" or "\r"
#check Nat -> Nat     -- alternative ASCII notation

#check Nat × Nat      -- type the product as "\times"
#check Prod Nat Nat   -- alternative notation

#check Nat → Nat → Nat
#check Nat → (Nat → Nat)  --  same type as above

#check Nat × Nat → Nat
#check (Nat → Nat) → Nat -- a "functional"

#check Nat.succ     -- Nat → Nat
#check (0, 1)       -- Nat × Nat
#check Nat.add      -- Nat → Nat → Nat

#check Nat.succ 2   -- Nat
#check Nat.add 3    -- Nat → Nat
#check Nat.add 5 2  -- Nat
#check (5, 9).1     -- Nat
#check (5, 9).2     -- Nat

#eval Nat.succ 2   -- 3
#eval Nat.add 5 2  -- 7
#eval (5, 9).1     -- 5
#eval (5, 9).2     -- 9

-- Types as objects

#check Nat               -- Type
#check Bool              -- Type
#check Nat → Bool        -- Type
#check Nat × Bool        -- Type
#check Nat → Nat         -- ...
#check Nat × Nat → Nat
#check Nat → Nat → Nat
#check Nat → (Nat → Nat)
#check Nat → Nat → Bool
#check (Nat → Nat) → Nat

/-
You can see that each one of the expressions above is an object of type Type. You can also declare new constants for types:
-/
namespace one
def α : Type := Nat
def β : Type := Bool
def F : Type → Type := List
def G : Type → Type → Type := Prod

#check α        -- Type
#check F α      -- Type
#check F Nat    -- Type
#check G α      -- Type → Type
#check G α β    -- Type
#check G α Nat  -- Type
end

namespace two
def α : Type := Nat
def β : Type := Bool

#check Prod α β       -- Type
#check α × β          -- Type

#check Prod Nat Nat   -- Type
#check Nat × Nat      -- Type
end


def α : Type := Nat
#check List α
#check List Nat

-- Given that every expression in Lean has a type, it is natural to ask: what type does Type itself have?
#check Type

/-
You have actually come up against one of the most subtle aspects of Lean's typing system.
Lean's underlying foundation has an infinite hierarchy of types:
-/
#check Type     -- Type 1
#check Type 1   -- Type 2
#check Type 2   -- Type 3
#check Type 3   -- Type 4
#check Type 4   -- Type 5

/-
Think of Type 0 as a universe of "small" or "ordinary" types. Type 1 is then a larger universe of types, which contains Type 0 as an element, and Type 2 is an even larger universe of types, which contains Type 1 as an element. The list is infinite: there is a Type n for every natural number n. Type is an abbreviation for Type 0.

Type is an abbreviation for Type 0.
-/

#check Type
#check Type 0


/-
Some operations, however, need to be polymorphic over type universes. For example, List α should make sense for any type α, no matter which type universe α lives in. This explains the type signature of the function List:
-/
#check List -- List.{u} (α : Type u) : Type u

/-
Here u is a variable ranging over type levels. The output of the #check command means that whenever α has type Type n, List α also has type Type n. The function Prod is similarly polymorphic:
-/
#check Prod

/-
To define polymorphic constants, Lean allows you to declare universe variables explicitly using the universe command:
-/

section
universe u

def F (α : Type u) : Type u := Prod α α

#check F    -- Type u → Type u
end

def F2.{u} (α : Type u) : Type u := Prod α α

#check F2    -- Type u → Type u


-- Function Abstraction and evaluation

#eval (fun x : Nat => x + 5) 10    -- 15
#eval (λ x : Nat => x + 5) 10    -- 15

#check fun x : Nat => fun y : Bool => if not y then x + 1 else x + 2
#check fun (x : Nat) (y : Bool) => if not y then x + 1 else x + 2
#check fun x y => if not y then x + 1 else x + 2   -- Nat → Bool → Nat

/-
Some mathematically common examples of operations of functions can be described in terms of lambda abstraction:
-/
def f (n : Nat) : String := toString n
def g (s : String) : Bool := s.length > 0

#check fun x : Nat => x        -- Nat → Nat
#check fun x : Nat => true     -- Nat → Bool
#check fun x : Nat => g (f x)  -- Nat → Bool
#check fun x => g (f x)        -- Nat → Bool

#check fun (g : String → Bool) (f : Nat → String) (x : Nat) => g (f x)
-- (String → Bool) → (Nat → String) → Nat → Bool

#check fun (α β γ : Type) (g : β → γ) (f : α → β) (x : α) => g (f x)




#eval (fun x : Nat => x) 1     -- 1
#eval (fun x : Nat => true) 1  -- true


def add (x y : Nat) :=
  x + y

def add_v2 (x : Nat) (y : Nat) :=
  x + y

#eval add 1 2
#eval add_v2 1 2


-- namespaces

namespace Foo
  def a : Nat := 5
  def f (x : Nat) : Nat := x + 7

  def fa : Nat := f a
  def ffa : Nat := f (f a)

  #check a
  #check f
  #check fa
  #check ffa
  #check Foo.fa
end Foo

-- #check a  -- error
-- #check f  -- error
#check Foo.a
#check Foo.f
#check Foo.fa
#check Foo.ffa

open Foo

#check a
#check f
#check fa
#check Foo.fa

-- What makes dependent type theory dependent?


#check @List.cons    -- {α : Type u_1} → α → List α → List α
#check @List.nil     -- {α : Type u_1} → List α
#check @List.length  -- {α : Type u_1} → List α → Nat
#check @List.append  -- {α : Type u_1} → List α → List α → List α


namespace aoijfdoijasdf
universe u v

def f (α : Type u) (β : α → Type v) (a : α) (b : β a) : (a : α) × β a :=
  ⟨a, b⟩

def g (α : Type u) (β : α → Type v) (a : α) (b : β a) : Σ a : α, β a :=
  Sigma.mk a b

def h1 (x : Nat) : Nat :=
  (f Type (fun α => α) Nat x).2

#eval h1 5 -- 5

def h2 (x : Nat) : Nat :=
  (g Type (fun α => α) Nat x).2

#eval h2 5 -- 5
end aoijfdoijasdf

universe u

section
  variable {α : Type u}
  variable (x : α)
  def ident := x
end

#check ident
#check ident 4
#check ident "hello"

#check @ident
