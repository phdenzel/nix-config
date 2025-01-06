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
  environment.systemPackages = with pkgs; [
    kitty
  ];
  programs.foot = {
    enable = mkDefault true;
    enableBashIntegration = mkDefault true;
    enableZshIntegration = mkDefault true;
  };
}
