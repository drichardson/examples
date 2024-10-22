import Lake
open Lake DSL

package "interlude" where
  version := v!"0.1.0"

@[default_target]
lean_exe "interlude" where
  root := `Main
