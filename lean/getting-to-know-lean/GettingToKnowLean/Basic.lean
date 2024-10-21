def hello := "world"

#eval 1 + 2

#eval 1 + 2 * 5

#eval String.append "Hello, " "Lean!"

#eval String.append "great " (String.append "oak " "tree")

#eval String.append "it is " (if 1 > 2 then "yes" else "no")

#eval String.append "it is "

#eval 42 + 19
#eval String.append "A" (String.append "B" "C")
#eval String.append (String.append "A" "B") "C"
#eval if 3 == 3 then 5 else 7
#eval if 3 == 4 then "equal" else "not equal"


#eval (1 - 2) /- using natural numbers and evaluates to 0, not -1 -/
#eval (1 - 2 : Int)

#check (1 - 2 : Nat)


#check String.append "hello" [" ", "world"]

/-
Functions and Definitions
https://lean-lang.org/functional_programming_in_lean/getting-to-know/functions-and-definitions.html
-/

def add1 (n : Nat) : Nat := n + 1
#eval add1 23

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

#eval maximum 3 2

#eval maximum (3 + 5) (2 + 1)

#check maximum
#check maximum 3
#check maximum 3 4


/-
Define the function joinStringsWith with type String -> String -> String -> String that creates a new string by placing its first argument between its second and third arguments. joinStringsWith ", " "one" "and another" should evaluate to "one, and another".
-/

def joinStringsWith (sep : String) (string1 : String) (string2 : String) : String :=
  String.append string1 (String.append sep string2)

#eval joinStringsWith ", " "one" "and another"

/- What is the type of joinStringsWith ": "? Check your answer with Lean. -/
#check joinStringsWith ": "

/- Define a function volume with type Nat → Nat → Nat → Nat that computes the volume of a rectangular prism with the given height, width, and depth. -/
def volume (h : Nat) (w : Nat) (l : Nat) : Nat := h * w * l

#eval volume 1 1 1
#eval volume 2 3 4

def Str : Type := String
def aStr : Str := "This is a string"

#check aStr
#check Str

def NaturalNumber : Type := Nat
def thirtyEight : NaturalNumber := 38 -- failed to synthesize

-- workaround 1
def thirtyEight_v2 : NaturalNumber := (38 : Nat)

#eval thirtyEight_v2

-- workaround 2
abbrev N : Type := Nat
def thirtyNine : N := 39
#eval thirtyNine

-- Structures https://lean-lang.org/functional_programming_in_lean/getting-to-know/structures.html

structure Point where
  x : Float
  y : Float
deriving Repr

def origin : Point := { x := 0.0, y := 0.0 }

#eval origin

#eval origin.x
#eval origin.y

def addPoints (p1 : Point) (p2 : Point) : Point :=
  { x := p1.x + p2.x, y := p1.y + p2.y }


#eval addPoints origin { x := 1.0, y := 2.0 }

def distance (p1 : Point) (p2 : Point) : Float :=
  Float.sqrt (((p2.x - p1.x)^2.0) + ((p2.y - p1.y)^2.0))


#eval distance {x := 1.0, y := 2.0} {x := 5.0, y := -1.0}

#eval { x := 0.0, y := 1.0 } -- type not known
#eval ({ x := 0.0, y := 1.0 } : Point) -- longhand
#eval { x := 0.0, y := 1.0 : Point} -- shorthand

-- Updating structures

def zeroX(p : Point) : Point :=
  { p with x := 0 }

#eval zeroX {x := 1.0, y := 2.0}

#check Point.x
#check Point.y
#check Point

-- constructors
#eval Point.mk 1.0 5.0
#check Point.mk
#check Point.point

structure Point2 where
  point ::
  x : Float
  y : Float
deriving Repr

#check Point2.mk
#check Point2.point
#eval Point2.point 1.0 2.0


def p1 : Point := {x := 1.0, y := 2.0}
#eval p1.x
#eval p1.y
#eval Point.x p1
#eval Point.y p1

#eval "one string".append " and another"

def Point.modifyBoth (f : Float → Float) (p : Point) : Point :=
  { x := f p.x, y := f p.y }

def p2 : Point := {x := 1.23, y := 4.56}

#eval Point.modifyBoth Float.floor p2
#eval p2.modifyBoth Float.floor

/- Define a structure named RectangularPrism that contains the height, width, and depth of a rectangular prism, each as a Float. -/
structure RectangularPrism where
  height : Float
  width : Float
  depth : Float
deriving Repr

#eval {height := 1.0, width := 2.0, depth := 3.0 : RectangularPrism}

def RectangularPrism.volume (v : RectangularPrism) : Float :=
  v.height * v.width * v.depth

#eval {height := 1.0, width := 2.0, depth := 3.0 : RectangularPrism}.volume

structure Segment where
  p1 : Point
  p2 : Point
  deriving Repr

--def Segment.length (s : Segment) : Float := distance s.p1 s.p2
def Segment.length (s : Segment) : Float := distance s.p1 s.p2

def seg1 := { p1 := {x := 1.0, y := 2.0}, p2 := {x := 1.0, y:= 3.0} : Segment}
#eval seg1.length

-- Which names are introduced by the declaration of RectangularPrism?
-- A: mk (the constructor) and the 3 automatically created accessors

-- Datatypes and Patterns
-- https://lean-lang.org/functional_programming_in_lean/getting-to-know/datatypes-and-patterns.html

-- Types such as structures are called product types.

def x := true

def isZero(n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => false

#eval isZero 1

#eval isZero (Nat.succ Nat.zero)
#eval Nat.succ Nat.zero

def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

#eval pred 12
#eval pred 1
#eval pred 0

def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

#eval even 0
#eval even 1
#eval even 2

-- note this fails because it would result in infinite recursion
def evenLoops (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (evenLoops n)


  def plus (n : Nat) (k : Nat) : Nat :=
    match k with
    | Nat.zero => n
    | Nat.succ k' => Nat.succ (plus n k')

  #eval plus 1 (Nat.succ Nat.zero)

def times (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => Nat.zero
  | Nat.succ k' => plus n (times n k')

def minus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => pred (minus n k')


  #eval times 2 3

  #eval minus 3 1
  #eval minus 3 100


-- not structurally recursive, won't compile
def div (n : Nat) (k : Nat) : Nat :=
if n < k then
  0
else Nat.succ (div (n - k) k)

structure PPoint (α : Type) where
  x : α
  y : α
deriving Repr

def natOrigin : PPoint Nat := { x := Nat.zero, y := Nat.zero }

#eval natOrigin


def replaceX (α : Type) (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

#check replaceX
#check replaceX Nat
#check replaceX Nat natOrigin
#check replaceX Nat natOrigin 5

#eval replaceX Nat natOrigin 5

inductive Sign where
  | pos
  | neg

def posOrNegThree (s : Sign) : match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

#check posOrNegThree Sign.pos
#check posOrNegThree Sign.neg

#eval posOrNegThree Sign.pos
#eval posOrNegThree Sign.neg


def primesUnder10 : List Nat := [2, 3, 5, 7]

#eval primesUnder10

#eval List.cons 123 List.nil
#eval [123]

#eval List.cons 1 (List.cons 2 List.nil)
#eval [1, 2]


def length (α : Type) (xs : List α) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons y ys => Nat.succ (length α ys)

#eval length Nat [1,2,3]

-- implicit arguments

def replaceX_v2 {α : Type} (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }


#eval replaceX_v2 natOrigin 6 -- no type parameter necessary, inferred.
#eval replaceX_v2 (α := Nat) natOrigin 6 -- explicitly set anyway

#eval Option.some 123
#eval Option.none (α := Nat)

inductive MyOption (α : Type) : Type where
  | none : MyOption α
  | some (val : α) : MyOption α
deriving Repr

#eval MyOption.some 123
#eval MyOption.none (α := Nat)

#eval List.head? (α := Nat) []
#eval List.head? (α := Nat) ["hi",1, 2, 3]

#eval [].head? -- error, type cannot be determined
#eval [].head? (α := Int) -- explicitly specify a type
#eval ([] : List Int).head? -- alternately specify type with with type annotation

-- Prod - the product type (like a 2-tuple or pair)
#eval Prod.mk 1 3

-- Product type and its constructors have a special syntax since they are used so much.
-- Prod α β is typically written with α × β.
-- can write × with \times in VS code

def fives : String × Int := { fst := "five", snd := 5 }
-- it suffices to write
def fives_v2 : String × Int := ("five", 5)
-- both notations are right-associative. This means the following are equivalent
def sevens : String × Int × Int := ("VII", 7, 4+3)
def sevens_v2 : String × (Int × Int) := ("VII", (7, 4+3))
-- in other words, all products of more than two types, and their corresponding constructors
-- are actually nested products and nested pairs behind the scenes.

-- Sum
-- inl means left injection and inr means right injection. These are called injections like
-- injective functions that preserve distinctness. Since the type is preserved in a sum
-- type, each value is an injection from the original type and value.
-- α ⊕ β can be written with \oplus and is shorthand for Sum α β.
def PetName : Type := String ⊕ String
/-
In a real program, it would usually be better to define a custom inductive datatype for this purpose with informative constructor names. Here, Sum.inl is to be used for dog names, and Sum.inr is to be used for cat names. These constructors can be used to write a list of animal names:
-/
def animals : List PetName :=
  [Sum.inl "Spot", Sum.inr "Tiger", Sum.inl "Fifi", Sum.inl "Rex", Sum.inr "Floof"]

#eval animals.head?

def howManyDogs (pets : List PetName) : Nat :=
  match pets with
  | [] => 0
  | Sum.inl _ :: morePets => howManyDogs morePets + 1
  | Sum.inr _ :: morePets => howManyDogs morePets

#eval howManyDogs animals

-- Unit

#eval Unit.unit
#eval ()
#check Unit.unit
#check ()

--

inductive MyType (α : Type) : Type where
| ctor : α → MyType α


--def howdy : ℕ → ℝ

def f1 (n : Nat) : Float := (Float.ofNat n) * 1.5

#eval f1 3

def f2 (n : Nat) (f : Float) : Float := (Float.ofNat n) * f

#eval f2 3 5.234

#check f2
#check f2 3

def f2_curry := f2 2
#check f2_curry
#eval f2_curry 3.52

def f2_no_args := f2_curry 3.52
#check f2_no_args
#eval f2_no_args

def f2_with_type_sig : Nat → Float → Float := f2
#check f2_with_type_sig
#eval f2_with_type_sig 2 2.53

/-
For technical reasons, allowing these datatypes could make it possible to undermine Lean's internal logic, making it unsuitable for use as a theorem prover.
-/
inductive MyType1423 : Type where
  | ctor : (MyType1423 → Int) → MyType1423

-- Write a function to find the last entry in a list. It should return an Option.
def list_last {α : Type} (xs : List α) : Option α :=
  match xs with
  | List.nil => Option.none
  | List.cons y List.nil => Option.some y
  | List.cons _y ys => list_last ys


def l1 := [1,2,3,4,5]

#eval list_last l1

/-
Write a function that finds the first entry in a list that satisfies a given predicate. Start the definition with def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
-/

def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | List.nil => Option.none
  | List.cons y ys =>
    if predicate y then
      Option.some y
    else
      List.findFirst? ys predicate

def mypred (n : Nat) : Bool := n == 2

#eval List.findFirst? l1 mypred
#eval List.findFirst? l1 (fun x => x == 2)
#eval List.findFirst? [1,3,5] mypred

/-
Write a function Prod.swap that swaps the two fields in a pair. Start the definition with def Prod.swap {α β : Type} (pair : α × β) : β × α :=
-/
def Prod.swap {α β : Type} (pair : α × β) : β × α := (pair.snd, pair.fst)

#eval (1.0, "String").swap

/-
Rewrite the PetName example to use a custom datatype and compare it to the version that uses Sum.
-/
inductive MyPetName where
  | dog : String → MyPetName
  | cat : String → MyPetName
deriving Repr

#eval MyPetName.dog "Odie"
#eval MyPetName.cat "Garfield"

def animals2 : List MyPetName :=
  [MyPetName.dog "Spot", MyPetName.cat "Tiger", MyPetName.cat "Fifi", MyPetName.dog "Rex", MyPetName.cat "Floof"]

def howManyDogs2 (pets : List MyPetName) : Nat :=
match pets with
| [] => 0
| List.cons (MyPetName.dog _) ys => howManyDogs2 ys + 1
| List.cons _ ys => howManyDogs2 ys

#eval howManyDogs2 animals2

/-
Write a function zip that combines two lists into a list of pairs. The resulting list should be as long as the shortest input list. Start the definition with def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=.
-/
def zip {α β : Type} (as : List α) (bs : List β) : List (α × β) :=
  match as, bs with
  | [], _ => ([] : List (Prod α β))
  | _, [] => []
  | a :: as', b :: bs' => (a,b) :: zip as' bs'

#eval zip [1,2,3] ["one", "two", "three"]
#eval zip [1,2] ["one", "two", "three"]
#eval zip [1,2,3] ["one", "two"]

/-
Write a polymorphic function take that returns the first n
 entries in a list, where n
 is a Nat. If the list contains fewer than n entries, then the resulting list should be the input list. #eval take 3 ["bolete", "oyster"] should yield ["bolete", "oyster"], and #eval take 1 ["bolete", "oyster"] should yield ["bolete"].
-/
def take {α : Type} (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | Nat.zero, _ => []
  | _, [] => []
  | Nat.succ k, y :: ys => y :: take k ys

#eval take 2 [1,2,3,4,5,6]
example : take 3 ["bolete", "oyster"] = ["bolete", "oyster"] := rfl
example : take 1 ["bolete", "oyster"] = ["bolete"] := rfl
/-
Using the analogy between types and arithmetic, write a function that distributes products over sums. In other words, it should have type α × (β ⊕ γ) → (α × β) ⊕ (α × γ).
-/
def distribute {α β γ : Type} (x : α × (β ⊕ γ)) : (α × β) ⊕ (α × γ) :=
  match x with
  | (a, Sum.inl b) => Sum.inl (a, b)
  | (a, Sum.inr y) => Sum.inr (a, y)

def val1 : Nat ⊕ String := Sum.inl 123
def val2 : Nat ⊕ String := Sum.inr "howdy"

#eval distribute (1, val1)
#eval distribute (1, val2)

/-
Using the analogy between types and arithmetic, write a function that turns multiplication by two into a sum. In other words, it should have type Bool × α → α ⊕ α.
-/

def multi {α : Type} (b : Bool) (val : α) : α ⊕ α :=
 match b with
 | true => Sum.inl val
 | false => Sum.inr val

#eval multi True 123
#eval multi False 123


-- Additional Conveniences
-- https://lean-lang.org/functional_programming_in_lean/getting-to-know/conveniences.html

-- Automatic Implicit Arguments

def length_verbose {α : Type} (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (length_verbose ys)

def length_implicit (xs : List α) : Nat :=
  match xs with
  | [] => 0
  | y :: ys => Nat.succ (length_implicit ys)

example : length_verbose [1,2,3] = 3 := rfl
example : length_implicit [1,2,3] = 3 := rfl

-- Pattern-Matching Definitions

def length_pattern_match : List α → Nat
  | [] => 0
  | y :: ys => Nat.succ (length_pattern_match ys)

example : length_pattern_match [1,2,3] = 3 := rfl


def drop : Nat → List α -> List α
  | Nat.zero, xs => xs
  | _, [] => []
  | Nat.succ n, _ :: xs => drop n xs

example : drop 2 [1,2,3,4] = [3,4] := rfl

-- named arguments and patterns in same definition
def fromOption (default : α) : Option α → α
  | none => default
  | some x => x

example : fromOption 2 (Option.some 123) = 123 := rfl
example : fromOption 2 Option.none  = 2 := rfl

-- this function is called Option.getD in the standard library

example : (some "salmonberry").getD "" = "salmonberry" := rfl
example : none.getD "" = "" := rfl

-- Local Definitions


-- as written unzip_slow is O(n^2) time complexity because of the double recursive call.
-- however, in my testing with #eval with lists around 30 items, which should take 2^30-ish amount of work, it returns very fast, making me think some sort of optimization is in place.
-- That said, lean provides ways to explicity keep the result with let.
def unzip_slow : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    (x :: (unzip_slow xys).fst, y :: (unzip_slow xys).snd)

example : unzip_slow [(1,"one")] = ([1], ["one"]) := rfl

def long_list := [
  (1,"one"), (2, "two"), (3, "three"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  (4, "four"), (5, "five"), (6, "six"),
  ]

#eval unzip_slow long_list

#eval 2 ^ (13 * 3)
#eval 2 ^ (20 * 3)

-- unzip_fast uses let to save result of the recursive call to unzip_fast, instead of calling it 2 times.

/-
In Lean, the result of the recursive call can be named, and thus saved, using let. Local definitions with let resemble top-level definitions with def: it takes a name to be locally defined, arguments if desired, a type signature, and then a body following :=. After the local definition, the expression in which the local definition is available (called the body of the let-expression) must be on a new line, starting at a column in the file that is less than or equal to that of the let keyword. For instance, let can be used in unzip like this:
-/
def unzip_fast : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped : List α × List β := unzip_fast xys
    (x :: unzipped.fst, y :: unzipped.snd)

  #eval unzip_fast long_list

/-
Local definitions with let may also use pattern matching when one pattern is enough to match all cases of a datatype. In the case of unzip, the result of the recursive call is a pair. Because pairs have only a single constructor, the name unzipped can be replaced with a pair pattern:
-/
def unzip_fast2 : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let (xs, ys) : List α × List β := unzip_fast2 xys
    (x :: xs, y :: ys)

#eval unzip_fast2 long_list

/-
The biggest difference between let and def is that recursive let definitions must be explicitly indicated by writing let rec. For instance, one way to reverse a list involves a recursive helper function, as in this definition:
-/
def reverse (xs : List α) : List α :=
  let rec helper : List α → List α → List α
    | [], soFar => soFar
    | y :: ys, soFar => helper ys (y :: soFar)
  helper xs []

example : reverse [1,2,3] == [3,2,1] := rfl

/-
Type Inference

In many situations, Lean can automatically determine an expression's type. In these cases, explicit types may be omitted from both top-level definitions (with def) and local definitions (with let). For instance, the recursive call to unzip does not need an annotation:
-/

def unzip_type_inf : List (α × β) → List α × List β
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped := unzip_type_inf xys
    (x :: unzipped.fst, y :: unzipped.snd)

example : unzip_type_inf [(1, "one"), (2, "two")] = ([1,2], ["one", "two"]) := rfl

-- Omitting the return type for unzip is possible when using an explicit match expression:

def unzip_omit_return (pairs : List (α × β)) :=
  match pairs with
  | [] => ([], [])
  | (x, y) :: xys =>
    let unzipped := unzip_omit_return xys
    (x :: unzipped.fst, y :: unzipped.snd)

example : unzip_omit_return [(1, "one"), (2, "two")] = ([1,2], ["one", "two"]) := rfl

/-
annotations. First off, explicit types communicate assumptions about the code to readers. Even if Lean can determine the type on its own, it can still be easier to read code without having to repeatedly query Lean for type information. Secondly, explicit types help localize errors. The more explicit a program is about its types, the more informative the error messages can be. This is especially important in a language like Lean that has a very expressive type system. Thirdly, explicit types make it easier to write the program in the first place. The type is a specification, and the compiler's feedback can be a helpful tool in writing a program that meets the specification. Finally, Lean's type inference is a best-effort system. Because Lean's type system is so expressive, there is no "best" or most general type to find for all expressions. This means that even if you get a type, there's no guarantee that it's the right type for a given application. For instance, 14 can be a Nat or an Int:
-/

#check 14
#check (14 : Int)

-- Missing type annotations can give confusing error messages.

/-
In general, messages that say something like "failed to infer" or that mention metavariables are often a sign that more type annotations are necessary. Especially while still learning Lean, it is useful to provide most types explicitly.
-/

/-
Simultaneous Matching

Pattern-matching expressions, just like pattern-matching definitions, can match on multiple values at once. Both the expressions to be inspected and the patterns that they match against are written with commas between them, similarly to the syntax used for definitions. Here is a version of drop that uses simultaneous matching:
-/

def drop_v2 (n : Nat) (xs : List α) : List α :=
  match n, xs with
  | Nat.zero, ys => ys
  | _, [] => []
  | Nat.succ n', _ :: ys => drop n' ys

example : drop_v2 2 [1,2,3,4] = [3,4] := rfl

/-
Natural Number Patterns

Just as there is special syntax to make list patterns more readable than using List.cons and List.nil directly, natural numbers can be matched using literal numbers and +. For instance, even can also be defined like this:
-/

def even_v2 : Nat → Bool
  | 0 => true
  | n + 1 => not (even_v2 n)

example : even_v2 0 == true := rfl
example : even_v2 1 == false := rfl
example : even_v2 2 == true := rfl

#print even_v2 10

--example : even_v2 19858761287341612873467812873468213581512303 == false := rfl
set_option maxRecDepth 512
example : even_v2 125 == false := rfl
set_option maxRecDepth 1000
example : even_v2 125 == false := rfl

example : even_v2 125123456789 == false := rfl

-- still too big, try tail call

def even_v2_tail (n : Nat) : Bool :=
  let rec aux : Nat → Bool → Bool
    | 0, acc => acc
    | n + 1, acc => aux n (not acc)
  aux n true

#print even_v2_tail

--set_option diagnostics true
example : even_v2_tail 1219 == false := rfl

-- #eval even_v2_tail 19858761287341612873467812873468213581512302

-- example : even_v2_tail 19858761287341612873467812873468213581512303 == false := rfl

/-
Natural Number Patterns

Just as there is special syntax to make list patterns more readable than using List.cons and List.nil directly, natural numbers can be matched using literal numbers and +. For instance, even can also be defined like this:
-/

def even_nat_num_pattern : Nat → Bool
| 0 => true
| n + 1 => not (even_nat_num_pattern n)

example : even_nat_num_pattern 0 = true := rfl
example : even_nat_num_pattern 1 = false := rfl
example : even_nat_num_pattern 2 = true := rfl
example : even_nat_num_pattern 225 = false := rfl

def halve : Nat → Nat
  | 0 => 0
  | 1 => 0
  | n + 2 => halve n + 1

example : halve 0 = 0 := rfl
example : halve 1 = 0 := rfl
example : halve 2 = 1 := rfl
example : halve 4 = 2 := rfl
example : halve 5 = 2 := rfl

/-
Anonymous Functions

This style of anonymous function expression is often referred to as a lambda expression, because the typical notation used in mathematical descriptions of programming languages uses the Greek letter λ (lambda) where Lean has the keyword fun. Even though Lean does permit λ to be used instead of fun, it is most common to write fun.
-/

#check fun x => x + 1
#check λ x => x + 1

#check fun (x : Int) => x + 1
#check fun {α : Type} (x : α) => x

#check fun x =>
  match x with
  | 0 => none
  | Nat.succ n => some n

#check fun
  | 0 => none
  | n + 1 => some n


def double (n : Nat) :=
  match n with
  | 0 => 0
  | k + 1 => double k + 2

def double_fun := fun
  | 0 => 0
  | k + 1 => double_fun k + 2

example : double 4 = 8 := rfl
example : double_fun 4 = 8 :=rfl

/-
When an anonymous function is very simple, like fun x => x + 1, the syntax for creating the function can be fairly verbose. In that particular example, six non-whitespace characters are used to introduce the function, and its body consists of only three non-whitespace characters. For these simple cases, Lean provides a shorthand. In an expression surrounded by parentheses, a centered dot character · can stand for an argument, and the expression inside the parentheses becomes the function's body. That particular function can also be written (· + 1).

The centered dot always creates a function out of the closest surrounding set of parentheses. For instance, (· + 5, 3) is a function that returns a pair of numbers, while ((· + 5), 3) is a pair of a function and a number. If multiple dots are used, then they become arguments from left to right:

Write · with \. or \centerdot. Note that \cdot does not work.
-/

example : (·, ·) 1 2 = (1,2) := rfl
example : (1, ·) 2 = (1,2) := rfl
example : (1, 2) = (1,2) := rfl

example : (fun x => x + x) 5 = 10 := rfl
example : (· * 2) 5 = 10 := rfl

/-
Namespaces

Each name in Lean occurs in a namespace, which is a collection of names. Names are placed in namespaces using ., so List.map is the name map in the List namespace. Names in different namespaces do not conflict with each other, even if they are otherwise identical. This means that List.map and Array.map are different names. Namespaces may be nested, so Project.Frontend.User.loginTime is the name loginTime in the nested namespace Project.Frontend.User.

Names can be directly defined within a namespace. For instance, the name double can be defined in the Nat namespace:
-/

def Nat.double (x : Nat) : Nat := x + x

/-
Because Nat is also the name of a type, dot notation is available to call Nat.double on expressions with type Nat:
-/

example : (4 : Nat).double = 8 := rfl

/-
In addition to defining names directly in a namespace, a sequence of declarations can be placed in a namespace using the namespace and end commands. For instance, this defines triple and quadruple in the namespace NewNamespace:
-/

namespace NewNamespace
def triple (x : Nat) : Nat := 3 * x
def quadruple (x: Nat) : Nat := 2 * x + 2 * x
#check triple
end NewNamespace

#check triple
#check NewNamespace.triple
#check NewNamespace.quadruple

-- Namespaces may be opened, which allows the names in them to be used without explicit qualification.

def timesTwelve (x : Nat) :=
  open NewNamespace in
  quadruple (triple x)

example : timesTwelve 2 = 24 := rfl

/-
Namespaces can also be opened prior to a command. This allows all parts of the command to refer to the contents of the namespace, rather than just a single expression. To do this, place the open ... in prior to the command.
-/
open NewNamespace in
#check quadruple
#check quadruple -- only applies to the first command

/- Namespaces may additionally be opened for all following commands for the rest of the file. To do this, simply omit the in from a top-level usage of open. -/

/-
if let

When consuming values that have a sum type, it is often the case that only a single constructor is of interest. For instance, given this type that represents a subset of Markdown inline elements:
-/

inductive Inline : Type where
  | lineBreak
  | string : String → Inline
  | emph : Inline -> Inline
  | strong : Inline → Inline
deriving Repr

#check Inline.lineBreak
#check Inline.string
#eval Inline.lineBreak
#eval Inline.string "howdy"
#eval Inline.emph (Inline.string "hi")

-- a function that recognizes string elements and extracts their contents can be written:

def Inline.string? : Inline -> Option String
  | Inline.string s => some s
  | _ => none

example : Inline.string? Inline.lineBreak = Option.none := rfl
example : Inline.string? (Inline.string "hi") = Option.some "hi" := rfl

-- An alternative way of writing this function body uses if together with let:

def Inline.string_if? (inline : Inline) : Option String :=
  if let Inline.string s := inline then
    some s
  else none

example : Inline.string_if? Inline.lineBreak = Option.none := rfl
example : Inline.string_if? (Inline.string "hi") = Option.some "hi" := rfl

/-
This is very much like the pattern-matching let syntax. The difference is that it can be used with sum types, because a fallback is provided in the else case. In some contexts, using if let instead of match can make code easier to read.
-/


/-
Positional Structure Arguments

The section on structures presents two ways of constructing structures:

The constructor can be called directly, as in Point.mk 1 2.
Brace notation can be used, as in { x := 1, y := 2 }.

In some contexts, it can be convenient to pass arguments positionally, rather than by name, but without naming the constructor directly. For instance, defining a variety of similar structure types can help keep domain concepts separate, but the natural way to read the code may treat each of them as being essentially a tuple. In these contexts, the arguments can be enclosed in angle brackets ⟨ and ⟩. A Point can be written ⟨1, 2⟩. Be careful! Even though they look like the less-than sign < and greater-than sign >, these brackets are different. They can be input using \< and \>, respectively.

Just as with the brace notation for named constructor arguments, this positional syntax can only be used in a context where Lean can determine the structure's type, either from a type annotation or from other type information in the program.
-/

#eval ⟨1,2⟩
#eval (⟨1,2⟩ : Point)

example : (⟨1, 2⟩ : Point) = { x := 1, y := 2 : Point} := rfl
example : ⟨1, 2⟩ = { x := 1, y := 2 : Point} := rfl

/-
String Interpolation
In Lean, prefixing a string with s! triggers interpolation, where expressions contained in curly braces inside the string are replaced with their values. This is similar to f-strings in Python and $-prefixed strings in C#. For instance,
-/

example : s!"three fives is {NewNamespace.triple 5}" = "three fives is 15" := rfl

/-
Not all expressions can be interpolated. For instance, attempting to interpolate a function fails.

This is because there is no standard way to convert functions into strings. The Lean compiler maintains a table that describes how to convert values of various types into strings, and the message failed to synthesize instance means that the Lean compiler didn't find an entry in this table for the given type. This uses the same language feature as the deriving Repr syntax that was described in the section on structures.
-/

#check s!"three fives is {NewNamespace.triple}"
