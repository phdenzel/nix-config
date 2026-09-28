# Shared Python environment for JupyterLab and the JupyterHub ML kernel.
# Usage: `import ./jupyterlab-env.nix {inherit pkgs inputs;}`;
# `ml = true` adds the ML kernel's extra packages.
{
  pkgs,
  inputs,
  ml ? false,
}: let
  inherit (pkgs) lib;
  rocmSupport = pkgs.config.rocmSupport or false;
  rocmTargets = pkgs.config.rocmTargets or [];

  # Stable, because unstable's torch 2.13 needs aotriton 0.12b and nixpkgs
  # ships 0.11.1b.
  stable = pkgs.stable.appendOverlays [
    (import ../../overlays/oskar.nix {inherit inputs;})
    (import ../../overlays/ska-ost-array-config.nix {inherit inputs;})
  ];

  python = stable.python313.override {
    packageOverrides = _: pyPrev: {
      torch =
        if !rocmSupport
        then pyPrev.torch
        else
          (pyPrev.torch.override {
            rocmSupport = true;
            gpuTargets = rocmTargets;
          })
          .overrideAttrs (oldAttrs: {
            # add_make_kernel_pt.sh runs as `bash <path>`; its /bin/bash
            # shebang does not exist in the sandbox.
            postPatch =
              (oldAttrs.postPatch or "")
              + ''
                patchShebangs --build aten/src/ATen/native/transformers/hip/flash_attn/ck
              '';
          });
    };
  };

  oskarpy = stable.oskarpy.override {
    python3Packages = python.pkgs;
  };

  chuchichaestli = python.pkgs.buildPythonPackage rec {
    pname = "chuchichaestli";
    version = "0.2.16";
    pyproject = true;
    build-system = with python.pkgs; [hatchling];
    propagatedBuildInputs = with python.pkgs; [
      numpy
      h5py
      torch
      torchvision
    ];
    src = stable.fetchPypi {
      inherit pname version;
      # dist = "py3";
      # python = "py3";
      sha256 = "sha256-7OGv0545CtpAkBw1V2dPrcJRgXqo7jGSbC4un3SIgIE=";
    };
    doCheck = false;
    meta = {
      description = "Where you find all the state-of-the-art cooking utensils (salt, pepper, gradient descent...  the usual).";
      license = stable.lib.licenses.gpl3Plus;
    };
  };
in
  python.withPackages (p:
    (with p; [
      jupyterlab
      ipykernel
      pip
      numpy
      scipy
      pandas
      scikit-learn
      matplotlib
      seaborn
      plotly
      h5py
      tqdm
      astropy
      gitpython
      torch
      torchvision
    ])
    ++ lib.optionals ml (with p; [
      torchinfo
      astropy-healpix
      pillow
      hydra-core
      diffusers
      ska-ost-array-config
      chuchichaestli
      oskarpy
    ]))
