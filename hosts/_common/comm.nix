{pkgs, lib, ...}: with lib; {
  environment.systemPackages = with pkgs; [
    signal-desktop
    slack
    teams
    whatsapp-for-linux
    zoom-us
  ];
}
