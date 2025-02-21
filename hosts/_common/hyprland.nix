{
  pkgs,
  lib,
  ...
}:
with lib; {
  programs.hyprland = {
    enable = true;
    xwayland.enable = true;
  };
  programs.waybar.enable = true;
  programs.hyprlock.enable = true;
  services.hypridle.enable = true;

  services.pipewire = {
    enable = true;
    wireplumber.enable = true;
  };
  security.rtkit.enable = true; # recommended for pipewire
  environment.systemPackages = with pkgs; [
    clipse
    grim
    hyprpaper
    hyprpicker
    hyprsunset
    kitty
    nwg-displays
    nwg-look
    rofi-wayland
    slurp
    swaynotificationcenter
    swayosd
    wlogout
    wlr-randr
    wl-clipboard
  ];
  programs.foot = {
    enable = mkDefault true;
    enableBashIntegration = mkDefault true;
    enableZshIntegration = mkDefault true;
  };
}
