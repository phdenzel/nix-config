{
  config,
  pkgs,
  lib,
  ...
}: let
  vikunja = config.services.vikunja.package;
  vikunjaUsers = ["phdenzel" "ldenzel" "rdenzel"];
  mkProvisionUser = username: ''
    ${vikunja}/bin/vikunja user create \
      --username ${username} \
      --email "$(cat ${config.sops.secrets."vikunja/${username}/email".path})" \
      --password "$(cat ${config.sops.secrets."vikunja/${username}/password".path})"
  '';

  provisionUsers = pkgs.writeShellScript "vikunja-provision-users" ''
    set -euo pipefail
    SENTINEL=/var/lib/vikunja/.provisioned

    if [ -f "$SENTINEL" ]; then
      echo "Vikunja users already provisioned, skipping."
      exit 0
    fi

    ${lib.concatMapStrings mkProvisionUser vikunjaUsers}

    touch "$SENTINEL"
  '';

in {
  services.vikunja = {
    enable = true;
    port = 3456;
    frontendScheme = "http";
    frontendHostname = "${config.networking.hostName}.home";
    database.type = "sqlite";
    settings = {
      service = {
        enableregistration = false;
        timezone = "Europe/Zurich";
        maxitemsperpage = 100;
      };
    };
  };

  systemd.services.vikunja-provision = {
    description = "Provision initial Vikunja users";
    after = ["vikunja.service"];
    wants = ["vikunja.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      User = "vikunja";
      RemainAfterExit = true;
      ExecStart = provisionUsers;
    };
  };

  sops-host.keys = lib.concatMap (u: [
    "vikunja/${u}/password"
    "vikunja/${u}/email"
  ]) vikunjaUsers;

  networking.firewall.allowedTCPPorts = [config.services.vikunja.port];
}
