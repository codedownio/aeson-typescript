{ mkDerivation, haskellPackages, nodePackages, stdenv
}:
with haskellPackages;
mkDerivation {
  pname = "aeson-typescript";
  version = "0.2.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers interpolate mtl template-haskell text
    th-abstraction unordered-containers
  ];
  testHaskellDepends = [
    aeson base bytestring containers directory filepath hspec
    interpolate mtl process template-haskell temporary text
    th-abstraction unordered-containers nodePackages.typescript
  ];
  homepage = "https://github.com/codedownio/aeson-typescript#readme";
  description = "Generate TypeScript definition files from your ADTs";
  license = stdenv.lib.licenses.bsd3;
}
