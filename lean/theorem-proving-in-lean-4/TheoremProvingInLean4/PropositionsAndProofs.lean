def Implies (p q : Prop) : Prop := p → q
#check And     -- Prop → Prop → Prop
#check Or      -- Prop → Prop → Prop
#check Not     -- Prop → Prop
#check Implies -- Prop → Prop → Prop

variable (p q r : Prop)
#check And p q                      -- Prop
#check Or (And p q) r               -- Prop
#check Implies (And p q) (And q p)  -- Prop

structure Proof (p : Prop) : Type where
  proof : p
#check Proof   -- Proof : Prop → Type

axiom and_comm (p q : Prop) : Proof (Implies (And p q) (And q p))
#check and_comm

variable (p q : Prop)
#check and_comm p q     -- Proof (Implies (And p q) (And q p))

/-
In addition to axioms, however, we would also need rules to build new proofs from old ones. For example, in many proof systems for propositional logic, we have the rule of modus ponens:
-/
axiom modus_ponens : (p q : Prop) → Proof (Implies p q) → Proof p → Proof q


axiom implies_intro : (p q : Prop) → (Proof p → Proof q) → Proof (Implies p q)

#check Prop
#check Sort 1
#check Sort 2


variable {p : Prop}
variable {q : Prop}

theorem t1 : p → q → p := fun hp : p => fun hq : q => hp

theorem t1_1 : p → q → p := fun hp : p => fun hq : q => hp

#print t1

-- can explictly specify final term with show statement
theorem t1_2 : p → q → p :=
  fun hp : p =>
  fun hq : q =>
  show p from hp

-- As with ordinary definitions, we can move the lambda-abstracted variables to the left of the colon:

theorem t1_3 (hp : p) (hq : q) : p := hp
#print t1_3


-- We can use the theorem t1 just as a function application:

axiom hp : p
theorem t2 : q → p := t1 hp

-- Declaring an "axiom" hp : p is tantamount to declaring that p is true, as witnessed by hp.

--axiom unsound : False
axiom unsound : False
-- everything follows from false
theorem ex : 1 = 0 :=
  False.elim unsound
