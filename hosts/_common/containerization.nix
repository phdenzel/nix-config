{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    podman
    podman-compose
    podman-desktop
  ];
}
