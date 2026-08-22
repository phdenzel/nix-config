{
  pkgs,
  lib,
  ...
}: let
  package = pkgs.protonmail-bridge;
  # Headless CLI, used for autostart on hosts without the GUI (macOS/servers).
  cmd = [
    (lib.getExe package)
    "--noninteractive"
  ];
  # On Linux the GUI (protonmail-bridge-gui) is installed and manages the bridge
  # lifecycle itself. Running the headless `--noninteractive` instance alongside
  # it makes the GUI report "an orphan instance is running" and refuse to start,
  # since it only ever wants a single bridge instance that it spawned. So on
  # Linux we autostart the GUI (in its tray) instead of the headless service.
  guiExe = lib.getExe' pkgs.protonmail-bridge-gui "protonmail-bridge-gui";
in {
  home.packages = [package];

  # Linux desktops: autostart the GUI.
  systemd.user.services = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    protonmail-bridge = {
      Unit = {
        Description = "ProtonMail Bridge";
        After = ["graphical-session.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        ExecStart = guiExe;
        Restart = "on-failure";
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };
  };

  # macOS: no GUI build available, so run the headless bridge as a background agent.
  launchd.agents.protonmail-bridge = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin {
    enable = true;
    config = {
      ProgramArguments = cmd;
      KeepAlive = true;
      ProcessType = "Background";
      RunAtLoad = true;
    };
  };
}
