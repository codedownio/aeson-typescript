{ callCabal2nix, overrideCabal, typescript }:

let pkg = callCabal2nix "aeson-typescript" ./. {};
in overrideCabal pkg (old: {
  testHaskellDepends = old.testHaskellDepends ++ [ typescript ];
})
