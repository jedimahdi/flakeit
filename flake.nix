{
  description = "Flake Template";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];
      systems = import inputs.systems;
      perSystem = { self', config, pkgs, ... }: {
        haskellProjects.default = {
          devShell = {
            hlsCheck.enable = false;
          };
          autoWire = [ "packages" "apps" "checks" ];
        };

        treefmt = {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = true;
          };
        };

        devShells.default = pkgs.mkShell {
          name = "flakeit";
          inputsFrom = [
            config.treefmt.build.devShell
            config.haskellProjects.default.outputs.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
          ];
        };
        packages.default = self'.packages.flakeit;
        apps.default = self'.apps.flakeit;
      };
    };
}
