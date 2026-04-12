{
  pkgs,
  config,
  lib,
  ...
}: let
  forgejoStore = "/data/store/forgejo";
  cfg = config.services.forgejo;
  forgejoUsers = [
    {
      username = "phdenzel";
      isAdmin = true;
    }
    {
      username = "ldenzel";
      isAdmin = false;
    }
    {
      username = "rdenzel";
      isAdmin = false;
    }
  ];
  mkProvisionUser = {
    username,
    isAdmin,
  }: ''
    ${pkgs.sudo}/bin/sudo -u ${cfg.user} ${cfg.package}/bin/forgejo admin user create \
      --username ${username} \
      --email "$(cat ${config.sops.secrets."forgejo/${username}/email".path})" \
      --password "$(cat ${config.sops.secrets."forgejo/${username}/password".path})" \
      ${lib.optionalString isAdmin "--admin"} \
      --config ${cfg.stateDir}/custom/conf/app.ini || true
  '';
  provisionUsers = pkgs.writeShellScript "forgejo-provision-users" ''
    set -euo pipefail
    SENTINEL=${cfg.stateDir}/.provisioned

    if [ -f "$SENTINEL" ]; then
      echo "Forgejo users already provisioned, skipping."
      exit 0
    fi

    ${lib.concatMapStrings mkProvisionUser forgejoUsers}

    touch "$SENTINEL"
  '';
in {
  services.forgejo = {
    enable = true;
    package = pkgs.forgejo;
    settings.server = {
      PROTOCOL = "http";
      DOMAIN = "${config.networking.hostName}.home";
      HTTP_PORT = 3022;
      SSH_PORT = 22;
    };
    lfs = {
      enable = true;
      contentDir = "${forgejoStore}/lfs";
    };
    dump = {
      enable = true;
      type = "tar.gz";
      interval = "01:30";
      backupDir = "${forgejoStore}/dump";
    };
    settings.mailer.ENABLED = true;
    secrets.mailer = {
      FROM = config.sops.secrets."forgejo/mailer/address".path;
      USER = config.sops.secrets."forgejo/mailer/address".path;
      PASSWD = config.sops.secrets."forgejo/mailer/password".path;
      PROTOCOL = config.sops.secrets."forgejo/mailer/protocol".path;
      SMTP_ADDR = config.sops.secrets."forgejo/mailer/smtp_addr".path;
      SMTP_PORT = config.sops.secrets."forgejo/mailer/smtp_port".path;
    };
  };

  systemd.services.forgejo-provision = {
    description = "Provision initial Forgejo users";
    after = ["forgejo.service"];
    wants = ["forgejo.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = provisionUsers;
      supplementaryGroups = [];
    };
  };

  # networking.firewall.allowedTCPPorts = [cfg.settings.server.HTTP_PORT];

  systemd.tmpfiles.rules = [
    "d ${forgejoStore}/lfs   02750  ${cfg.user}  ${cfg.group}  - -"
    "d ${forgejoStore}/dump  02750  ${cfg.user}  ${cfg.group}  - -"
  ];

  sops-host.keys =
    lib.concatMap ({username, ...}: [
      "forgejo/${username}/email"
      "forgejo/${username}/password"
    ])
    forgejoUsers
    ++ [
      "forgejo/mailer/address"
      "forgejo/mailer/password"
      "forgejo/mailer/protocol"
      "forgejo/mailer/smtp_addr"
      "forgejo/mailer/smtp_port"
    ];
}
