{
  description = "aeson-typescript";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.gitignore = {
    url = "github:hercules-ci/gitignore.nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/release-26.05";

  outputs = { self, flake-utils, gitignore, haskellNix, nixpkgs, nixpkgsMaster }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        # compiler-nix-name = "ghc966";
        compiler-nix-name = "ghc984";
        # compiler-nix-name = "ghc9101";

        pkgs = import nixpkgs {
          inherit system;
          overlays = [haskellNix.overlay];
          inherit (haskellNix) config;
        };

        pkgsMaster = import nixpkgsMaster {
          inherit system;
        };

        src = gitignore.lib.gitignoreSource ./.;

        flake = (pkgs.haskell-nix.hix.project {
          inherit src compiler-nix-name;
          evalSystem = system;
          projectFileName = "stack.yaml";
          modules = [];
        }).flake {};

        flakeWindows = (pkgs.pkgsCross.mingwW64.haskell-nix.hix.project {
          inherit src compiler-nix-name;
          evalSystem = system;
          # projectFileName = "stack.yaml";
          projectFileName = "stack-9.8.4.yaml";
          # projectFileName = "stack-9.10.1.yaml";
          modules = [{
            reinstallableLibGhc = false;
          }];
        }).flake {};

      in
        {
          packages = {
            inherit (pkgs.haskell.packages.${compiler-nix-name}) weeder;

            inherit flake;

            normal = flake.packages."aeson-typescript:lib:aeson-typescript";
            windows = flakeWindows.packages."aeson-typescript:lib:aeson-typescript";

            test = pkgs.writeShellScriptBin "stack-test" ''
              export NIX_PATH=nixpkgs=${pkgs.path}
              ${pkgs.stack}/bin/stack test
            '';

            nixpkgsPath = pkgs.writeShellScriptBin "nixpkgsPath.sh" "echo -n ${pkgs.path}";
          };

          devShells.default = pkgs.mkShell {
            buildInputs = [
              pkgs.typescript
              pkgsMaster.haskell.compiler.ghc9124
              # (pkgsMaster.haskell-language-server.override { supportedGhcVersions = ["9124"]; })
            ];
          };
        });
}
