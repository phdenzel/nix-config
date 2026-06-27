# JupyterLab environment + always-on launchd agent. Import on darwin hosts only.
{
  pkgs,
  config,
  ...
}: let
  jlabEnv = import ./jupyterlab-env.nix pkgs;
  port = "8000";
  userName = "phdenzel";
  homeDir = "/Users/${userName}";

  # Generated via python -c 'from jupyter_server.auth import passwd; print(passwd())'
  passwordFile = config.sops.secrets."jupyterlab/password".path;

  # Set the password and disable the random token
  jupyterConfig = pkgs.writeText "jupyter_server_config.py" ''
    c.PasswordIdentityProvider.hashed_password = open("${passwordFile}").read().strip()
    c.IdentityProvider.token = ""
  '';
in {
  environment.systemPackages = [jlabEnv];

  # Decrypt the jupyter password hash with the host ssh key (the darwin sops
  # backend's default age.sshKeyPaths), owned by the user, readable by the agent.
  sops.secrets."jupyterlab/password" = {
    sopsFile = ../secrets.yaml;
    owner = userName;
    mode = "0400";
  };

  # Single-user JupyterLab on localhost:${port}, managed by launchd.
  launchd.user.agents.jupyter.serviceConfig = {
    ProgramArguments = [
      "${jlabEnv}/bin/jupyter-lab"
      "--no-browser"
      "--ip=127.0.0.1"
      "--port=${port}"
      "--notebook-dir=${homeDir}"
      "--config=${jupyterConfig}"
    ];
    RunAtLoad = true;
    KeepAlive = true;
    StandardOutPath = "${homeDir}/Library/Logs/jupyter-lab.log";
    StandardErrorPath = "${homeDir}/Library/Logs/jupyter-lab.log";
  };
}
