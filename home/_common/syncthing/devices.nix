{...}:
let
  devices = (import ./_devices.nix).definitions;
in {
  services.syncthing.settings.devices = devices;
}
