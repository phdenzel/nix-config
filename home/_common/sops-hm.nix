{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.sops-hm;
in {
  options.sops-hm = {
    enable = mkEnableOption "Enable sops-nix secrets extraction for home-manager user.";
    user = mkOption {
      description = "Username for which sops-nix extracts secrets.";
      type = types.str;
      default = "phdenzel";
    };
    usersRoot = mkOption {
      description = "Home root, i.e. /home for linux or /Users for macOS.";
      type = types.str;
      default = "/home";
    };
    secretsFileRoot = mkOption {
      description = "Root path of the secrets file from which sops-nix extracts secrets.";
      type = types.path;
      # full file path will be ../${cfg.user}/secrets.yaml
      default = ../.;
    };
    ageFile = mkOption {
      description = "AGE keyfile used by sops-nix to decrypt secrets.";
      type = types.str;
      default = "${cfg.usersRoot}/${cfg.user}/.config/sops/age/keys.txt";
    };
    secrets = mkOption {
      description = "Generic secrets to decrypt.";
      type = types.listOf types.str;
      default = [];
    };
    templates = mkOption {
      description = "Additional secrets templates to decrypt.";
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    sops = {
      defaultSopsFile = cfg.secretsFileRoot + "/${cfg.user}/secrets.yaml";

      age.keyFile = mkDefault "${cfg.ageFile}";

      secrets =
        {
          
        }
        // attrsets.mergeAttrsList (
          lists.map (name: {
            "${name}" = {};
          })
          cfg.secrets
        )
        // attrsets.mergeAttrsList (
          lists.map (name: {
            "${name}" = {};
          })
          cfg.templates
        );
    };
  };
}
