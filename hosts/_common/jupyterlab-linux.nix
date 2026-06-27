# JupyterLab environment + always-on systemd user service. Import on NixOS only.
{
  pkgs,
  lib,
  ...
}: let
  jlabEnv = import ./jupyterlab-env.nix pkgs;
  port = "8000";

  # Password source for the login page.
  #   passwordFile != null -> read the argon2 hash from that file at runtime
  #     (set it to a sops secret to keep the hash out of /nix/store, e.g. add
  #      `config` to the module args and:
  #        passwordFile = config.sops.secrets."jupyter/password".path; )
  #   passwordFile == null -> use the inline hashedPassword below.
  # Generate a hash with:
  #   ${jlabEnv}/bin/python3 -c 'from jupyter_server.auth import passwd; print(passwd())'
  passwordFile = null;
  hashedPassword = "argon2:$argon2id$v=19$m=10240,t=10,p=8$REPLACE_ME";
  pwValue =
    if passwordFile != null
    then ''open("${passwordFile}").read().strip()''
    else ''"${hashedPassword}"'';

  # Set the password and disable the random token so the password is the sole
  # credential on the login page.
  jupyterConfig = pkgs.writeText "jupyter_server_config.py" ''
    c.PasswordIdentityProvider.hashed_password = ${pwValue}
    c.IdentityProvider.token = ""
  '';
in {
  environment.systemPackages = [jlabEnv];

  # Single-user JupyterLab on localhost:${port}, managed by systemd (%h = home).
  systemd.user.services.jupyter = {
    description = "JupyterLab";
    wantedBy = ["default.target"];
    serviceConfig = {
      ExecStart = lib.concatStringsSep " " [
        "${jlabEnv}/bin/jupyter-lab"
        "--no-browser"
        "--ip=127.0.0.1"
        "--port=${port}"
        "--notebook-dir=%h"
        "--config=${jupyterConfig}"
      ];
      Restart = "on-failure";
    };
  };
}
