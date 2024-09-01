let
    pkgs = import <nixpkgs> {};
in
derivation {
    name = "simple";
    builder = "${pkgs.bash}/bin/bash";
    args = [ ./builder.sh ];
    gcc = pkgs.gcc;
    coreutils = pkgs.coreutils;
    src = ./simple.c;
    system = builtins.currentSystem;
}