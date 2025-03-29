{
  pkgs,
  lib,
  ...
}:
with lib; {
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
    withUWSM = true;
  };
  programs.hyprlock.enable = true;
  services.hypridle.enable = true;

  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
  };
  security.rtkit.enable = true; # recommended for pipewire

  environment.systemPackages = with pkgs; [
    clipse
    (flameshot.override {enableWlrSupport = true;})
    grim
    hyprpaper
    hyprpicker
    hyprpolkitagent
    hyprsunset
    kitty
    nwg-displays
    nwg-look
    pyprland
    rofi-wayland
    slurp
    swaynotificationcenter
    swayosd
    uwsm
    waybar
    wlogout
    wlr-randr
    wl-clipboard
  ];
  programs.foot = {
    enable = mkDefault true;
    enableBashIntegration = mkDefault true;
    enableZshIntegration = mkDefault true;
  };

  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
  };
}
