{pkgs, ...}: {
  services = {
    jupyterhub.enable = true;
    jupyterhub.jupyterhubEnv = pkgs.python312.withPackages (p: with p; [
      jupyterhub
      jupyterhub-systemdspawner
    ]);
    jupyterhub.jupyterlabEnv = pkgs.python312.withPackages (p: with p; [
      jupyterhub
      jupyterlab
    ]);
    jupyterhub.port = 8000;
  };
}
