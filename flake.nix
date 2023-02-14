{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = { };
      };
      # TODO: Generalize and move to a new flake-parts module (or haskell-flake)
      flake.haskellFlakeProjectModules = rec {
        # Overlay consumed by this flake.
        input = { pkgs, ... }: {
          imports = [
            # Flake input dependencies go here.
          ];
          # Haskell overrides go here.
          overrides = self: super: { };
        };
        # Overlay exposed by this flake.
        output = { pkgs, lib, ... }: withSystem pkgs.system (ctx@{ config, ... }: {
          # Pass along the dependency overrides to consuming flake.
          imports = [
            input
          ];
          # Pass local packages, as overlay, to the consuming flake.
          source-overrides =
            lib.mapAttrs (name: ks: ks.root)
              config.haskellProjects.default.packages;
        });
      };
    });
}
