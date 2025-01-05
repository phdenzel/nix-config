{
  config,
  lib,
  inputs,
  ...
}: with lib; let
  sopsHost = "${config.networking.hostName}";
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  sops = {
    defaultSopsFile = mkDefault ../secrets.yaml;
    validateSopsFiles = mkDefault false;

    age = {
      sshKeyPaths = mkDefault [ "/etc/ssh/ssh_host_ed25519_key" ];
      # keyFile = "/var/lib/sops-nix/key.txt";
      # generateKey = true;
    };

    secrets = {
      "passwd/${sopsHost}" = {
        neededForUsers = mkDefault true;
        sopsFile = ../secrets.yaml;
      };
    };
  };
}
