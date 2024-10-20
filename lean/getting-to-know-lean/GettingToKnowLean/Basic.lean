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
