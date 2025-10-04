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
    jupyterhub.kernels = let
      remote-chuchichaestli = builtins.fetchurl {
        url = "https://raw.githubusercontent.com/CAIIVS/chuchichaestli/refs/heads/main/default.nix";
        sha256 = "sha256:0a838l8h2qv4c95zi68r1nr8ndmn8929f53js04g3h15ii3zbskb";
      };
      chuchichaestli = pkgs.callPackage remote-chuchichaestli {
        src = pkgs.fetchFromGitHub {
          owner = "CAIIVS";
          repo = "chuchichaestli";
          rev = "main";
          sha256 = "sha256:0l5q6j7kav2lsy1pl1izqa8f31q32r7fz47qhim45gjawp838vrw";
        };
      };
    in {
      python3 = let
        env = (pkgs.python313.withPackages (p: with p; [
          ipykernel
          pip
          numpy
          scipy
          torch
          torchvision
          h5py
          tqdm
          astropy
          pillow
          matplotlib
          psutil
          gitpython
          hydra-core
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
