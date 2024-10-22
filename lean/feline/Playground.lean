/-
Some of this is from:
https://lean-lang.org/functional_programming_in_lean/hello-world/step-by-step.html

And some is from:
https://lean-lang.org/functional_programming_in_lean/getting-to-know/conveniences.html
-/

def twice (action : IO Unit) : IO Unit := do
  action
  action

-- () is equivalent to Unit.unit
#eval ()
#eval Unit.unit

#eval twice (IO.println "shy")

def nTimes (action : IO Unit) : Nat → IO Unit
  | 0 => pure ()
  | n + 1 => do
    action
    nTimes action n

#eval nTimes (IO.println "nTimes?") 3

def countdown : Nat → List (IO Unit)
  | 0 => [IO.println "Blast off!"]
  | n + 1 => IO.println s!"{n + 1}" :: countdown n


def from5 : List (IO Unit) := countdown 5


#eval from5.length

def runActions : List (IO Unit) → IO Unit
  | [] => pure ()
  | act :: actions => do
    act
    runActions actions

#eval runActions [
  IO.println "first",
  IO.println "second"
  ]

#eval do
    IO.println "first"
    IO.println "second"

#eval runActions (countdown 3)
