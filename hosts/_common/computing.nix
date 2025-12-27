{pkgs, ...}: {
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
    jupyterhub.kernels = let
      chuchichaestli = pkgs.python313Packages.buildPythonPackage rec {
        pname = "chuchichaestli";
        version = "0.2.16";
        pyproject = true;
        build-system = with pkgs.python313Packages; [hatchling];
        propagatedBuildInputs = with pkgs.python313Packages; [
          numpy
          h5py
          torch
          torchvision
        ];
        src = pkgs.fetchPypi {
          inherit pname version;
          # dist = "py3";
          # python = "py3";
          sha256 = "sha256-7OGv0545CtpAkBw1V2dPrcJRgXqo7jGSbC4un3SIgIE=";
        };
        doCheck = false;
        meta = {
          description = "Where you find all the state-of-the-art cooking utensils (salt, pepper, gradient descent...  the usual).";
          license = pkgs.lib.licenses.gpl3Plus;
        };
      };
    in {
      python3 = let
        env = (pkgs.python313.withPackages (p: with p; [
          ipykernel
          pip
          numpy
          scipy
          pandas
          torch
          torchvision
          torchinfo
          h5py
          tqdm
          astropy
          pillow
          matplotlib
          seaborn
          plotly
          gitpython
          hydra-core
          diffusers
          chuchichaestli
        ]));
      in {
        displayName = "Python3 for ML";
        argv = [
          "${env.interpreter}"
          "-m"
          "ipykernel_launcher"
          "-f"
          "{connection_file}"
        ];
        language = "python";
        logo32 = "${env}/${env.sitePackages}/ipykernel/resources/logo-32x32.png";
        logo64 = "${env}/${env.sitePackages}/ipykernel/resources/logo-64x64.png";
      };
    };
  };
  security.pam.services.jupyterhub.enableGnomeKeyring = true;
}
