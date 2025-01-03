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

  # Make stable nixpkgs (declared in the flake inputs as nixpkgs-stable)
  # accessible through 'pkgs.stable'
  stable-packages = final: _prev: {
    stable = import inputs.nixpkgs-stable {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
