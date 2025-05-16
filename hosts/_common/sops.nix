{
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  cfg = config.sops-host;
  hostName = "${config.networking.hostName}";
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  options.sops-host = {
    enable = mkEnableOption "Enable sops-nix secrets extraction for host.";
    host = mkOption {
      description = "Hostname for which sops-nix extracts secrets.";
      type = types.str;
      default = hostName;
    };
    secretsFile = mkOption {
      description = "Path of the secrets file from which sops-nix extracts secrets.";
      type = types.path;
      # full file path will be ../${cfg.user}/secrets.yaml
      default = ../secrets.yaml;
    };
    keyFiles = mkOption {
      description = "SSH keyfile used by sops-nix to decrypt secrets.";
      type = types.listOf types.str;
      default = ["/etc/ssh/ssh_host_ed25519_key"];
    };
    keys = mkOption {
      description = "Additional secret keys to decrypt.";
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    sops = {
      defaultSopsFile = mkDefault cfg.secretsFile;
      validateSopsFiles = mkDefault false;
      age.sshKeyPaths = mkDefault cfg.keyFiles;

      secrets = {
        "passwd/${sopsHost}" = {
          neededForUsers = true;
          sopsFile = cfg.secretsFile;
        };
      } //
      attrsets.mergeAttrsList (
        lists.map (name: {
          "${name}" = {};
        })
        cfg.keys
      );
    };
  };
}
