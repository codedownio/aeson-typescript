with import <nixpkgs> { config.allowUnfree = true; };

callPackage (import ./package.nix) { mkDerivation = haskellPackages.mkDerivation;
}
