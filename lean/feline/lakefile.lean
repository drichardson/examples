import Lake
open Lake DSL

package "feline" where
  version := v!"0.1.0"

@[default_target]
lean_exe "feline" where
  root := `Main
