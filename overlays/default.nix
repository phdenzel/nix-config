# This file defines overlays
{
  outputs,
  inputs,
  ...
}: let
  addPatches = pkg: patches:
    pkg.overrideAttrs (oldAttrs: {
      patches = (oldAttrs.patches or []) ++ patches;
    });
in {
  # Add custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # Change package versions, add patches, set compilation flags, etc.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = addPatches prev.example [./example.diff];
  };

  # Alias inputs.nixpkgs-stable to pkgs.stable,
  # set system and allow unfree
  stable-packages = final: _prev: {
    stable = import inputs.nixpkgs-stable {
      system = final.stdenv.hostPlatform.system;
      config.allowUnfree = true;
    };
  };

  # For every flake input, create pkgs.inputs.${flake} alias from
  # 'inputs.${flake}.packages.${pkgs.system}' or
  # 'inputs.${flake}.legacyPackages.${pkgs.system}'
  flake-inputs = final: _: {
    inputs =
      builtins.mapAttrs (
        _: flake: let
          legacyPackages = (flake.legacyPackages or {}).${final.stdenv.hostPlatform.system} or {};
          packages = (flake.packages or {}).${final.stdenv.hostPlatform.system} or {};
        in
          if legacyPackages != {}
          then legacyPackages
          else packages
      )
      inputs;
  };
}
