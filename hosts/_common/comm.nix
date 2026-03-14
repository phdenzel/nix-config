{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    signal-desktop
    slack
    teams-for-linux
    wasistlos
    webex
    zoom-us
  ];
}
