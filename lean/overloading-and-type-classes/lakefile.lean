import Lake
open Lake DSL

package "overloading-and-type-classes" where
  version := v!"0.1.0"

@[default_target]
lean_exe "overloading-and-type-classes" where
  root := `Main
