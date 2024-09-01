{}:
  derivation {
    # ...
    name = "basic-test";
    builder = "/bin/bash";
    system = builtins.currentSystem;
    args = [ "-c" "echo hello world > $out" ];
    # ...
  }