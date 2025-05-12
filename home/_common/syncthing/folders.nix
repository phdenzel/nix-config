{lib, ...}:
with lib; let
  syncs = import ./_folders.nix;
  devices = (import ./_devices.nix).groups;
in {
  services.syncthing.settings.folders =
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          enable = mkDefault true;
          path = "~/${name}";
          devices = devices.all;
          ignorePerms = false;
        };
      })
      syncs.all
    ) //
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          enable = mkDefault true;
          path = "~/${name}";
          devices = devices.home;
          ignorePerms = false;
        };
      })
      syncs.home
    ) //
    attrsets.mergeAttrsList (
      lists.map (name: {
        "${name}" = {
          enable = mkDefault true;
          path = "~/${name}";
          devices = devices.work;
          ignorePerms = false;
        };
      })
      syncs.work
    );
}
