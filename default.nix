let nixpkgs = import <nixpkgs> {};
in
with nixpkgs;

callPackage (import ./package.nix)
  { callCabal2nix = haskellPackages.callCabal2nix;
    overrideCabal = haskell.lib.overrideCabal;
    typescript = nodePackages.typescript;
  }
