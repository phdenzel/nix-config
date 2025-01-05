{
  lib,
  config,
  inputs,
  ...
}:
with lib; let
  cfg = config.sops-user;
in {
  imports = [inputs.sops-nix.nixosModules.sops];

  options.sops-user = {
    enable = mkEnableOption "Enable sops-nix secrets extraction for user.";
    user = mkOption {
      description = "Username for which sops-nix extracts secrets.";
      type = types.str;
      default = "phdenzel";
    };
    host = mkOption {
      description = "Hostname for which sops-nix extracts secrets.";
      type = types.str;
      default = "phinix";
    };
    secretsFileRoot = mkOption {
      description = "Root path of the secrets file from which sops-nix extracts secrets.";
      type = types.path;
      # full file path will be ../${cfg.user}/secrets.yaml
      default = ../.;
    };
    keyFile = mkOption {
      description = "SSH keyfile used by sops-nix to decrypt secrets.";
      type = types.str;
      # full file path will be ../${cfg.user}/secrets.yaml
      default = "/etc/ssh/ssh_host_ed25519_key";
    };
    sshKeys = mkOption {
      description = "Additional SSH keys to decrypt.";
      type = types.listOf types.str;
      default = [];
    };
  };

  config = mkIf cfg.enable {
    sops = {
      defaultSopsFile = cfg.secretsFileRoot + "/${cfg.user}/secrets.yaml";

      age = {
        sshKeyPaths = mkDefault ["${cfg.keyFile}"];
        # keyFile = "/var/lib/sops-nix/key.txt";
        # generateKey = true;
      };

      secrets =
        {
          "passwd/${cfg.user}/${cfg.host}".neededForUsers = true;
        }
        // attrsets.mergeAttrsList (
          lists.map (name: {
            "ssh_keys/${cfg.user}/${cfg.host}/${name}" = {
              path = "/home/${cfg.user}/.ssh/${name}";
              owner = "${cfg.user}";
            };
            "ssh_keys/${cfg.user}/${cfg.host}/${name}.pub" = {
              path = "/home/${cfg.user}/.ssh/${name}.pub";
              owner = "${cfg.user}";
            };
          }) cfg.sshKeys
        );
    };
  };
}
