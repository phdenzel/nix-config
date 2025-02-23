{
  lib,
  config,
  ...
}:
with lib; let
  cfg = config.sops-user;
in {
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
    gpgKeys = mkOption {
      description = "Additional GPG keys to decrypt.";
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
              path = "${cfg.usersRoot}/${cfg.user}/.ssh/${name}";
              owner = "${cfg.user}";
              mode = "0400";
            };
            "ssh_keys/${cfg.user}/${cfg.host}/${name}.pub" = {
              path = "${cfg.usersRoot}/${cfg.user}/.ssh/${name}.pub";
              owner = "${cfg.user}";
              mode = "0400";
            };
          })
          cfg.sshKeys
        )
        // attrsets.mergeAttrsList (
          lists.map (name: {
            "gpg_keys/${cfg.user}/${cfg.host}/${name}.asc" = {
              path = "${cfg.usersRoot}/${cfg.user}/.gnupg/${name}.asc";
              owner = "${cfg.user}";
              mode = "0400";
            };
            "gpg_keys/${cfg.user}/${cfg.host}/${name}.public.asc" = {
              path = "${cfg.usersRoot}/${cfg.user}/.gnupg/${name}.public.asc";
              owner = "${cfg.user}";
              mode = "0400";
            };
          })
          cfg.gpgKeys
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
