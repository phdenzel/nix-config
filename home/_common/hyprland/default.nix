{...}: {

  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = false;  # conflicts with UWSM
  };

  imports = [
    # ../waybar
    ../clipse
    ../swaync
    ../swayosd
    ./exec-once.nix
    ./settings.nix
    ./rules.nix
    ./binds.nix
    # ./hyprpolkitagent.nix
    # ./hyprpaper.nix
    # ./hyprlock.nix
    # ./hypridle.nix
    ./pyprland.nix
  ];
}
