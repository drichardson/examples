import Lake
open Lake DSL

package "lean-manual" where
  version := v!"0.1.0"

lean_lib «LeanManual» where
  -- add library configuration options here

@[default_target]
lean_exe "lean-manual" where
  root := `Main
