{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    karere
    signal-desktop
    slack
    teams-for-linux
    webex
    zoom-us
  ];
}
