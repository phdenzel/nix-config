{...}: {

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = false;  # conflicts with UWSM
  };

  home.sessionVariables = {
    QT_QPA_PLATFORM = "wayland";
    SDL_VIDEODRIVER = "wayland";
    XDG_SESSION_TYPE = "wayland";
    USE_WAYLAND_GRIM = 1;
  };

  imports = [
    ../clipse
    ../flameshot
    ../swaync
    ../swayosd
    ../waybar
    ../wlogout
    ./env.nix
    ./exec-once.nix
    ./settings.nix
    ./rules.nix
    ./binds.nix
    ./pyprland.nix
    ./hyprpolkitagent.nix
    ./hyprcursor.nix
    ./hyprpaper.nix
    ./hyprlock.nix
    ./hypridle.nix
  ];
}
