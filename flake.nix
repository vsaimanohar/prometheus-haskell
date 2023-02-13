{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = { };
      };
      flake.haskellFlakeProjectModules.output = { pkgs, ... }: {
        source-overrides = {
          prometheus-client = ./prometheus-client;
          prometheus-proc = ./prometheus-proc;
          prometheus-metrics-ghc = ./prometheus-metrics-ghc;
          wai-middleware-prometheus = ./wai-middleware-prometheus;
          prometheus-haskell-example = ./example;
        };
      };
    };
}
