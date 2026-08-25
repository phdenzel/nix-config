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

  # Radio astronomical simulation tools
  oskar = import ./oskar.nix {inherit inputs;};
  ska-ost-array-config = import ./ska-ost-array-config.nix {inherit inputs;};

  # Declarative Rust toolchains (provides pkgs.fenix)
  rust = inputs.fenix.overlays.default;

  # Change package versions, add patches, set compilation flags, etc.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = addPatches prev.example [./example.diff];
    glances = prev.glances.overrideAttrs (oldAttrs:
      if final.stdenv.hostPlatform.isAarch64
      then {
        doCheck = false;
        doInstallCheck = false;
        nativeCheckInputs = [];
        checkInputs = [];
      }
      else {
        # The RESTful tests boot a glances web server on localhost:61235 and
        # poll it; the server never comes up in the Nix sandbox, so every
        # request dies with ECONNREFUSED.
        disabledTestPaths = (oldAttrs.disabledTestPaths or []) ++ ["tests/test_restful.py"];
      });

    # Python package fixes (applied to every interpreter's package set).
    pythonPackagesExtensions =
      (prev.pythonPackagesExtensions or [])
      ++ [
        (pyFinal: pyPrev: {
          # aider-chat-full with rocmSupport cause re-build
          spacy = pyPrev.spacy.overrideAttrs (_: {
            doCheck = false;
            doInstallCheck = false;
          });
        })
      ];
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
