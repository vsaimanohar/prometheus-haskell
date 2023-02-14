{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
      systems = nixpkgs.lib.systems.flakeExposed;
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = { };
      };
      flake.haskellFlakeProjectModules.output = { pkgs, lib, ... }: withSystem pkgs.system (ctx@{ config, ... }: {
        source-overrides =
          lib.mapAttrs (name: ks: ks.root)
            config.haskellProjects.default.packages;
      });
    });
}
