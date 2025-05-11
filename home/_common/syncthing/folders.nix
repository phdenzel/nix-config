{lib, ...}:
with lib; let
  syncs = import ./_folders.nix;
  devices = attrNames (import ./_devices.nix);
in {
  services.syncthing.settings.folders =
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          enable = mkDefault true;
          path = "~/${name}";
          devices = devices;
          ignorePerms = false;
        };
      })
      syncs
    );
}
