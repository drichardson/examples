import Lake
open Lake DSL

package "getting-to-know-lean" where
  version := v!"0.1.0"

lean_lib «GettingToKnowLean» where
  -- add library configuration options here

@[default_target]
lean_exe "getting-to-know-lean" where
  root := `Main
