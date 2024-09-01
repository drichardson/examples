# I took the commit from nixos-24.05 from https://status.nixos.org/
let
pkgs = import (fetchTarball (import "https://github.com/NixOS/nixpkgs/archive/6e99f2a27d600612004fbd2c3282d614bfee6421.tar.gz"));
in
pkgs