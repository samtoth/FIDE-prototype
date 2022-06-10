let
  nixpkgs = import <nixpkgs> {};
  pkgs = import ./default.nix {};
in
  pkgs.shellFor{
    buildInputs = with nixpkgs.haskellPackages; [
      hlint
      ghcid
    ];
  }
