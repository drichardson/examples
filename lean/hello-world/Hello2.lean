/-
the do syntax can be thought of as a special-purpose sub-language within Lean that is dedicated to modeling imperative programs
-/

def main : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout

  stdout.putStrLn "How would you like to be addressed?"
  let input ← stdin.getLine
  let name := input.dropRightWhile Char.isWhitespace

  stdout.putStrLn s!"Hello {name}!"
