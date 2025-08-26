{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    podman
    podman-compose
  ];
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
  };
  
}
