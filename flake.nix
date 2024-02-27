{
  description = "Flake It";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs";
        flake-utils.follows = "utils";
      };
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, utils, treefmt-nix, pre-commit-hooks, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages.extend (final: prev: {
          flakeit = prev.callCabal2nix "flakeit" ./. { };
        });
        flakeit = pkgs.lib.pipe haskellPackages.flakeit [
          pkgs.haskell.lib.justStaticExecutables
          pkgs.haskell.lib.dontCheck
          pkgs.haskell.lib.dontHaddock
          (haskellPackages.generateOptparseApplicativeCompletions [ "flakeit" ])
        ];
        treefmtEval = treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            fourmolu.enable = true;
            cabal-fmt.enable = true;
          };
        };
        preCommit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            treefmt.enable = true;
          };
          settings.treefmt.package = treefmtEval.config.build.wrapper;
        };
      in
      {
        packages = {
          default = flakeit;
          inherit flakeit;
          flakeit-sdist = pkgs.haskell.lib.sdistTarball flakeit;
        };
        devShells.default =
          haskellPackages.shellFor {
            packages = p: [ p.flakeit ];
            inherit (preCommit) shellHook;
            withHoogle = true;
            nativeBuildInputs = [ treefmtEval.config.build.wrapper pkgs.ghcid ]
              ++ (pkgs.lib.attrValues treefmtEval.config.build.programs);
          };
      });
}
