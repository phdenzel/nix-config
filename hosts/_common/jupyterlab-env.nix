# Shared JupyterLab Python environment.
# Imported by the platform-specific jupyterlab-{darwin,linux}.nix modules so the
# package list lives in one place. Usage: `import ./jupyterlab-env.nix pkgs`.
pkgs:
pkgs.python313.withPackages (p:
  with p; [
    jupyterlab
    ipykernel
    pip
    numpy
    scipy
    pandas
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
