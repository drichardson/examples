let
pinned-version = "https://github.com/NixOS/nixpkgs/archive/6e99f2a27d600612004fbd2c3282d614bfee6421.tar.gz";
pkgs = import (fetchTarball pinned-version) {};
in
pkgs.mkShell {
    packages = [
        pkgs.coreutils
    ];

    shellHook = ''
    echo hi
    '';
}
