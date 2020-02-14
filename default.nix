let nixpkgs = import <nixpkgs> {};
in
with nixpkgs;

callPackage (import ./package.nix) { nixpkgs = nixpkgs; }
