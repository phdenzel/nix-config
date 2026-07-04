{
  pkgs,
  lib,
  ...
}: let
  package = pkgs.protonmail-bridge;
  cmd = [
    (lib.getExe package)
    "--noninteractive"
  ];
in {
  home.packages = [package];

  systemd.user.services.protonmail-bridge = {
    Unit = {
      Description = "ProtonMail Bridge";
      After = ["graphical-session.target"];
    };
    Service = {
      ExecStart = lib.concatStringsSep " " cmd;
      Restart = "always";
    };
    Install = {
      WantedBy = ["graphical-session.target"];
    };
  };

  launchd.agents.protonmail-bridge = {
    enable = true;
    config = {
      ProgramArguments = cmd;
      KeepAlive = true;
      ProcessType = "Background";
      RunAtLoad = true;
    };
  };
}
