{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  customGhc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    lens
    vty
    brick
  ]);
in
[ customGhc ] ++ 
[
  # Add normal packages here
]
