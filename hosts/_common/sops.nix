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
    ownedKeys = mkOption {
      description = "Additional owned secret keys to decrypt (owner is at root).";
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
        "passwd/${cfg.host}" = {
          neededForUsers = true;
          sopsFile = cfg.secretsFile;
        };
      } //
      attrsets.mergeAttrsList (
        lists.map (name: {
          "${name}" = {
            sopsFile = cfg.secretsFile;
          };
        })
        cfg.keys
      ) //
      attrsets.mergeAttrsList (
        lists.map (name: {
          "${name}" = {
            sopsFile = cfg.secretsFile;
            mode = "0400";
            owner = let
              group = (elemAt (lib.strings.splitString "/" "${name}") 0);
            in
              if (hasAttr group config.users.groups)
              then group
              else "root";
          };
        })
        cfg.ownedKeys
      );
    };
  };
}
