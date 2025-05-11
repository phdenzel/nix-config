{config, lib, ...}:
with lib; let
  cfg = config.syncthingSpec;
in {
  options.syncthingSpec = {
    enable = mkEnableOption "Enable syncthing specialization module.";
    deactivatedFolders = mkOption {
      description = "Folder not to be shared on current machine.";
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    services.syncthing.settings.folders =
      attrsets.mergeAttrsList (
        lists.map (name: {
          "${name}".enable = mkForce false;
        })
        cfg.deactivatedFolders
      );
  };
}
