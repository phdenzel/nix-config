{
  lib,
  config,
  ...
}: with lib; let
  cfg = config.sops-user;
in {

  options.sops-config = {
    enable = mkEnableOption "Enable sops-nix secrets extraction for user.";
    user = mkOption {
      description = "Username for which sops-nix extracts secrets.";
      type = types.str;
      default = "phdenzel";
    };
    host = mkOption {
      description = "Hostname for which sops-nix extracts secrets.";
      type = types.str;
      default = "${config.networking.hostName}";
    };
    secretsFileRoot = mkOption {
      description = "Root path of the secrets file from which sops-nix extracts secrets.";
      type = types.str;
      # full file path will be ../${sops-config.user}/secrets.yaml
      default = "..";
    };
    keyFile = mkOption {
      description = "SSH keyfile used by sops-nix to decrypt secrets.";
      type = types.str;
      # full file path will be ../${sops-config.user}/secrets.yaml
      default = "/etc/ssh/ssh_host_ed25519_key";
    };
  };

  config = mkIf cfg.enable {
    # sops-nix.nixosModules.sops-nix import is handled in hosts/_common/sops.nix
    sops = {
      defaultSopsFile = builtins.toPath "${cfg.secretsFileRoot}/${cfg.user}/${cfg.secretsFilename}";
      validateSopsFiles = false;

      age = {
        sshKeyPaths = [ "${cfg.keyFile}" ];
        # keyFile = "/var/lib/sops-nix/key.txt";
        # generateKey = true;
      };

      secrets = {
        "passwd/${cfg.user}" = {
          neededForUsers = true;
        };
        "ssh_keys/${cfg.host}" = {
          path = "/home/${cfg.user}/.ssh/";
          owner = "${cfg.user}";
        };
      };
    };
  };
}
