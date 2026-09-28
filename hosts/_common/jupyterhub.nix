{
  pkgs,
  inputs,
  ...
}: let
  mlEnv = import ./jupyterlab-env.nix {
    inherit pkgs inputs;
    ml = true;
  };
in {
  services = {
    jupyterhub.enable = true;
    jupyterhub.extraConfig = ''
      c.Authenticator.allow_all = True
      c.PAMAuthenticator.admin_groups = {'wheel'}
    '';
    jupyterhub.jupyterhubEnv = pkgs.python313.withPackages (p:
      with p; [
        jupyterhub
        jupyterhub-systemdspawner
      ]);
    jupyterhub.jupyterlabEnv = pkgs.python313.withPackages (p:
      with p; [
        jupyterhub
        jupyterlab
      ]);
    jupyterhub.port = 8000;
    jupyterhub.kernels = {
      python3 = {
        displayName = "Python3 for ML";
        argv = [
          "${mlEnv.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
        logo32 = "${mlEnv}/${mlEnv.sitePackages}/ipykernel/resources/logo-32x32.png";
        logo64 = "${mlEnv}/${mlEnv.sitePackages}/ipykernel/resources/logo-64x64.png";
      };
    };
  };
  security.pam.services.jupyterhub.enableGnomeKeyring = true;
}
