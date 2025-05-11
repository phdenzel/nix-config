{...}:
let
  devices = import ./_devices.nix;
in {
  services.syncthing.settings.devices = devices;
}
