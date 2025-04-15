{pkgs, ...}: {
  services = {
    jupyterhub.enable = true;
    jupyterhub.extraConfig = ''
      c.Authenticator.allow_all = True
      c.PAMAuthenticator.admin_groups = {'wheel'}
    '';
    jupyterhub.jupyterhubEnv = pkgs.python312.withPackages (p:
      with p; [
        jupyterhub
        jupyterhub-systemdspawner
      ]);
    jupyterhub.jupyterlabEnv = pkgs.python312.withPackages (p:
      with p; [
        jupyterhub
        jupyterlab
      ]);
    jupyterhub.port = 8000;
  };
  security.pam.services.jupyterhub.enableGnomeKeyring = true;
}
