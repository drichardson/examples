import Lake
open Lake DSL

package "theorem-proving-in-lean-4" where
  version := v!"0.1.0"

lean_lib «TheoremProvingInLean4» where
  -- add library configuration options here

@[default_target]
lean_exe "theorem-proving-in-lean-4" where
  root := `Main
