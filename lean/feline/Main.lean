def bufsize : USize := 20 * 1024

/-
The dump function is declared partial, because it calls itself recursively on input that is not immediately smaller than an argument. When a function is declared to be partial, Lean does not require a proof that it terminates.

NOTE: When an if expression occurs as a statement in a do, as in dump, each branch of the if is implicitly provided with a do.

There is no danger of running out of stack space while calling dump because the recursive call happens as the very last step in the function, and its result is returned directly rather than being manipulated or computed with. This kind of recursion is called tail recursion, and it is described in more detail later in this book.
-/
partial def dump (stream : IO.FS.Stream) : IO Unit := do
  let buf ← stream.read bufsize
  if buf.isEmpty then
    pure ()
  else
    let stdout ← IO.getStdout
    stdout.write buf
    dump stream


def main2 : IO Unit := do
  let stdin ← IO.getStdin
  dump stdin

def fileStream (filename : System.FilePath) : IO (Option IO.FS.Stream) := do
  let fileExists ← filename.pathExists
  if not fileExists then
    let stderr ← IO.getStderr
    stderr.putStrLn s!"File not found: {filename}"
    pure none
  else
    let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
    pure (some (IO.FS.Stream.ofHandle handle))

/-
Note: process does not need to be marked partial because it is structurally recursive. Each recursive call is provided with the tail of the input list, and all Lean lists are finite. Thus, process does not introduce any non-termination.
-/

def process (exitCode : UInt32) (args : List String) : IO UInt32 := do
  match args with
  | [] => pure exitCode
  | "-" :: args =>
    let stdin ← IO.getStdin
    dump stdin
    process exitCode args
  | filename :: args =>
    let stream ← fileStream ⟨filename⟩
    match stream with
    | none =>
      process 1 args
    | some stream =>
      dump stream
      process exitCode args

def usage : IO UInt32 := do
  let stdout ← IO.getStdout
  stdout.putStrLn "Usage: feline [--help] [FILE1 FILE2 ...]"
  pure 0

def main (args : List String) : IO UInt32 :=
  match args with
  | [] => process 0 ["-"]
  | ["--help"] => usage
  | _ => process 0 args
