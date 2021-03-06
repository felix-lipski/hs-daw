{ pkgs ? import <nixpkgs> { } }:
with pkgs;
let
  customGhc = haskellPackages.ghcWithPackages (pkgs: with pkgs; [
    lens
    vty
    brick

    binary
    typed-process
    async

    pipes
    pipes-concurrency
  ]);
in
[ customGhc ] ++ 
[
  # Add normal packages here
]
