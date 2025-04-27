{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    signal-desktop-bin
    slack
    teams-for-linux
    whatsapp-for-linux
    zoom-us
  ];
}
