{ nixpkgs
}:

with nixpkgs;

let pkg = haskellPackages.callCabal2nix "aeson-typescript" ./. {};
in haskell.lib.overrideCabal pkg (old: {
  testHaskellDepends = old.testHaskellDepends ++ [ nodePackages.typescript ];
})
