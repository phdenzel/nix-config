{
  config,
  pkgs,
  ...
}: let
  vikunja = config.services.vikunja.package;
  mkProvisionUser = {
    username,
    email,
    passwordSecret,
  }: ''
    ${vikunja}/bin/vikunja user create \
      --username ${username} \
      --email ${email} \
      --password "$(cat ${passwordSecret})"
  '';

  provisionUsers = pkgs.writeShellScript "vikunja-provision-users" ''
    set -euo pipefail
    SENTINEL=/var/lib/vikunja/.provisioned

    if [ -f "$SENTINEL" ]; then
      echo "Vikunja users already provisioned, skipping."
      exit 0
    fi

    ${mkProvisionUser {
      username = "phdenzel";
      email = config.sops.secrets."vikunja/phdenzel/email".path;
      passwordSecret = config.sops.secrets."vikunja/phdenzel/password".path;
    }}
    ${mkProvisionUser {
      username = "ldenzel";
      email = config.sops.secrets."vikunja/ldenzel/email".path;
      passwordSecret = config.sops.secrets."vikunja/ldenzel/password".path;
    }}
    ${mkProvisionUser {
      username = "rdenzel";
      email = config.sops.secrets."vikunja/rdenzel/email".path;
      passwordSecret = config.sops.secrets."vikunja/rdenzel/password".path;
    }}

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

  sops-host.keys = [
    "vikunja/phdenzel"
    "vikunja/ldenzel"
    "vikunja/rdenzel"
  ];

  networking.firewall.allowedTCPPorts = [config.services.vikunja.port];
}
