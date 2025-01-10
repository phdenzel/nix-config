{
  config,
  lib,
  inputs,
  ...
}:
with lib; let
  sopsHost = "${config.networking.hostName}";
in {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  sops = {
    defaultSopsFile = mkDefault ../secrets.yaml;
    validateSopsFiles = mkDefault false;
    age.sshKeyPaths = mkDefault ["/etc/ssh/ssh_host_ed25519_key"];

    secrets = {
      "passwd/${sopsHost}" = {
        neededForUsers = true;
        sopsFile = ../secrets.yaml;
      };
    };
  };
}
