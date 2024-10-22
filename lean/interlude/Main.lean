def main : IO Unit :=
  IO.println s!"Hello, world!"

def woodlandCritters : List String :=
  ["hedgehog", "deer", "snail"]

def hedgehog := woodlandCritters[0]
def deer := woodlandCritters[1]
def snail := woodlandCritters[2]

/-
failed to prove index is valid, possible solutions:
  - Use `have`-expressions to prove the index is valid
  - Use `a[i]!` notation instead, runtime check is perfomed, and 'Panic' error message is produced if index is not valid
  - Use `a[i]?` notation instead, result is an `Option` type
  - Use `a[i]'h` notation instead, where `h` is a proof that index is valid
⊢ 3 < List.length woodlandCritters

This error message is saying Lean tried to automatically mathematically prove that 3 < List.length woodlandCritters, which would mean that the lookup was safe, but that it could not do so. Out-of-bounds errors are a common class of bugs, and Lean uses its dual nature as a programming language and a theorem prover to rule out as many as possible.

Understanding how this works requires an understanding of three key ideas: propositions, proofs, and tactics.
-/

def oops := woodlandCritters[3]


/-
A proposition is a statement that can be true or false. All of the following are propositions:

- 1 + 1 = 2
- Addition is commutative
- There are infinitely many prime numbers

In Lean, propositions are in fact types. They specify what counts as evidence that the statement is true. The proposition is proved by providing this evidence. On the other hand, if the proposition is false, then it will be impossible to construct this evidence.

For example, the proposition "1 + 1 = 2" can be written directly in Lean. The evidence for this proposition is the constructor rfl, which is short for reflexivity:

-/

def onePlusOneIsTwo : 1 + 1 = 2 := rfl

-- On the other hand, rfl does not prove the false proposition "1 + 1 = 15":

def onePlusOneIsFifteen : 1 + 1 = 15 := rfl

/-
When a proposition has been proven, it is called a theorem. In Lean, it is conventional to declare theorems with the theorem keyword instead of def. This helps readers see which declarations are intended to be read as mathematical proofs, and which are definitions. Generally speaking, with a proof, what matters is that there is evidence that a proposition is true, but it's not particularly important which evidence was provided. With definitions, on the other hand, it matters very much which particular value is selected—after all, a definition of addition that always returns 0 is clearly wrong.

The prior example could be rewritten as follows:

-/
def OnePlusOneIsTwo : Prop := 1 + 1 = 2

theorem onePlusOneIsTwo_theorem : OnePlusOneIsTwo := rfl


/-
Proofs are normally written using tactics, rather than by providing evidence directly. Tactics are small programs that construct evidence for a proposition. These programs run in a proof state that tracks the statement that is to be proved (called the goal) along with the assumptions that are available to prove it. Running a tactic on a goal results in a new proof state that contains new goals. The proof is complete when all goals have been proven.

To write a proof with tactics, begin the definition with by. Writing by puts Lean into tactic mode until the end of the next indented block. While in tactic mode, Lean provides ongoing feedback about the current proof state. Written with tactics, onePlusOneIsTwo is still quite short:
-/
theorem onePlusOneIsTwo_tactics : 1 + 1 =  2 := by simp

/-
Connectives
The basic building blocks of logic, such as "and", "or", "true", "false", and "not", are called logical connectives.
-/
--theorem addAndAppend : 1 + 1 = 2 ∧ ("Str".append "ing") = "String" := rfl
-- theorem addAndAppend : (1 + 1 = 2) ∧ (("Str".append "ing") = "String") := by simp
theorem addAndAppend : String.append "one" "two" = "onetwo" := by simp[String.append]

#eval "Str".append "ing" = "String"


theorem andImpliesOr : A ∧ B → A ∨ B :=
  fun andEvidence =>
    match andEvidence with
    | And.intro a _ => Or.inl a

-- The simp tactic can prove theorems that use these connectives. For example:

theorem onePlusOneAndLessThan : 1 + 1 = 2 ∧ 3 < 5 := by simp
theorem notTwoEqualFive : ¬ (1 + 1 = 5) := by simp
theorem trueIsTrue : True := True.intro
theorem trueOrFalse : True ∨ False := by simp
theorem falseImpliesTrue : False → True := by simp

/-
Evidence as Arguments
-/

def third (xs : List α) (ok : xs.length > 2) : α := xs[2]
#eval third woodlandCritters (by simp[woodlandCritters])


/-
Indexing Without Evidence

In cases where it's not practical to prove that an indexing operation is in bounds, there are other alternatives. Adding a question mark results in an Option, where the result is some if the index is in bounds, and none otherwise. For example:
-/

def thirdOption (xs : List α) : Option α := xs[2]?

#eval thirdOption woodlandCritters
#eval thirdOption ["only", "two"]

-- There is also a version that crashes the program when the index is out of bounds, rather than returning an Option:
#eval woodlandCritters[1]!


/-
Prove the following theorems using rfl: 2 + 3 = 5, 15 - 8 = 7, "Hello, ".append "world" = "Hello, world". What happens if rfl is used to prove 5 < 18? Why?
-/
theorem twoPlusThreeIsFive : 2 + 3 = 5 := rfl
theorem fifteenMinusEightIsSeven : 15 - 8 = 7 := rfl
theorem helloWorld : "Hello, ".append "world" = "Hello, world" := rfl
theorem eightLessThanfifteen : 8 < 15 := rfl -- rfl is for a = b

/-
Prove the following theorems using by simp: 2 + 3 = 5, 15 - 8 = 7, "Hello, ".append "world" = "Hello, world", 5 < 18.
-/
theorem ex2_a : 2 + 3 = 5 := by simp
theorem ex2_b : 15 - 8 = 7 := by simp
theorem ex2_c : "Hello, ".append "world" = "Hello, world" := by simp[String.append]
theorem ex2_d : 5 < 18 := by simp

/-
Write a function that looks up the fifth entry in a list. Pass the evidence that this lookup is safe as an argument to the function.
-/
def fifth (xs : List α) (ok : xs.length > 4) := xs[4]

#eval fifth [1,2,3,4,5] (by simp)
