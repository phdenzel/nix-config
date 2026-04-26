{pkgs, config, lib, ...}: let
  cfg = config.services.opencloud;
  cloudData = "/data/store/opencloud";  # PosixFS root
  # only single filesystem roots possible so bind-mount others
  extraRoots = [
    # the uuid unfortunately has to be provisioned by opencloud manually first
    # when installing, regenerate the uuids within opencloud, then adjust the configuration
    {name = "Media"; path = "/data/media"; uuid = "83e3c7f4-ca2a-415e-bda4-556d8360295b";}
    {name = "Shared"; path = "/data/documents"; uuid = "92fb3411-2edb-4663-a115-01949abcb581";}
    {name = "Backups"; path = "/data/backups"; uuid = "23cc9dfe-6379-4783-82fa-17534f0ac835";}
  ];
  # phdenzel is provisioned as admin
  adminRoleId = "71881883-c3aa-4921-abd9-f03a71f3c0e6";
  opencloudUsers = [
    {username = "phdenzel";}
    {username = "ldenzel";}
    {username = "rdenzel";}
  ];
  mkProvisionUser = {username}: ''
    USER_PASS=$(cat ${config.sops.secrets."opencloud/${username}/password".path})
    USER_EMAIL=$(cat ${config.sops.secrets."opencloud/${username}/email".path})
    HTTP_CODE=$(${pkgs.curl}/bin/curl -s -o /tmp/oc-provision-${username}.json -w "%{http_code}" \
      -u "admin:$ADMIN_PASS" \
      -X POST "$BASE_URL/graph/v1.0/users" \
      -H "Content-Type: application/json" \
      -d "{
        \"onPremisesSamAccountName\": \"${username}\",
        \"displayName\": \"${username}\",
        \"mail\": \"$USER_EMAIL\",
        \"passwordProfile\": {\"password\": \"$USER_PASS\"}
      }")
    echo "Provision ${username}: HTTP $HTTP_CODE - $(cat /tmp/oc-provision-${username}.json)"
  '';
  provisionUsers = pkgs.writeShellScript "opencloud-provision-users" ''
    set -euo pipefail
    SENTINEL="${cfg.stateDir}/.provisioned"

    if [ -f "$SENTINEL" ]; then
      echo "OpenCloud users already provisioned, skipping."
      exit 0
    fi

    ADMIN_PASS=$(cat ${config.sops.secrets."opencloud/admin/password".path})
    BASE_URL="http://127.0.0.1:${toString cfg.port}"

    # Wait for OpenCloud to be ready
    for i in $(seq 1 30); do
      ${pkgs.curl}/bin/curl -sf "$BASE_URL/health/ready" > /dev/null 2>&1 && break
      echo "Waiting for OpenCloud to be ready... ($i/30)"
      sleep 5
    done

    # Wait for IDM/IDP to finish initializing.
    sleep 5

    ${lib.concatMapStrings mkProvisionUser opencloudUsers}

    touch "$SENTINEL"
  '';
in {
  services.opencloud = {
    enable = true;
    port = 9200;
    address = "127.0.0.1";
    url = "https://opencloud.home";
    environment = {
      # no need for TLS with traefik reverse proxy
      PROXY_TLS = "false";
      # no TLS for internal calls
      PROXY_ENABLE_BASIC_AUTH = "true";
      OC_INSECURE = "true";
      # storage as standard POSIX file tree
      STORAGE_USERS_DRIVER = "posix";
      # file tree root
      STORAGE_USERS_POSIX_ROOT = cloudData;
      # saver if multiple services access filesystem
      STORAGE_USERS_POSIX_WATCH_FS = "true";
      # nats jetstream key-value store instead of default in-memory
      STORAGE_USERS_ID_CACHE_STORE = "nats-js-kv";
      # address of the nats jetstream instance
      STORAGE_USERS_ID_CACHE_STORE_NODES = "localhost:9233";
      # recommended to be the same as OC_EVENTS_ENDPOINT
      OC_CACHE_STORE_NODES = "localhost:9233";
      # recommended to be the same as OC_CACHE_STORE_NODES
      OC_EVENTS_ENDPOINT = "localhost:9233";
    };
    # Contains: INITIAL_ADMIN_PASSWORD=<phdenzel-password>
    environmentFile = config.sops.secrets."opencloud/env".path;
  };

  systemd.services.opencloud = {
    path = [ pkgs.inotify-tools ];
    serviceConfig.ReadWritePaths = [
      cloudData
    ] ++ builtins.map (r: r.path) extraRoots;
  };
  users.users.opencloud.extraGroups = [ "media" "users" "storage" ];

  systemd.tmpfiles.rules = [
    "d ${cloudData}            02775  phdenzel    ${cfg.user}  - -"
    "d ${cloudData}/projects   02775  ${cfg.user} ${cfg.user}  - -"
    "d /data/documents         02775  phdenzel    media  - -"
    "d /data/backups           02770  phdenzel    media  - -"
  ] ++ builtins.map (r:
    "d ${cloudData}/projects/${if r.uuid != "" then r.uuid else r.name}/${r.name}  02775  ${cfg.user}  ${cfg.user}  - -"
  ) extraRoots;

  # system.activationScripts.docPermissions = ''
  #   find /data/documents -type d -exec chmod 02770 {} +
  # '';

  fileSystems = lib.listToAttrs (builtins.map (r: {
    name = "${cloudData}/projects/${if r.uuid != "" then r.uuid else r.name}/${r.name}";
    value = {
      device = r.path;
      fsType = "none";
      options = [ "bind" "nofail" "x-systemd.after=systemd-tmpfiles-setup.service" ];
    };
  }) extraRoots);

  systemd.services.opencloud-provision = {
    description = "Provision initial OpenCloud users";
    after = ["opencloud-init-config.service"];
    wants = ["opencloud-init-config.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = provisionUsers;
      TimeoutStartSec = "infinity";
    };
  };

  sops-host.keys = [
    "opencloud/env"  # INITIAL_ADMIN_PASSWORD
    "opencloud/admin/password"
    "opencloud/phdenzel/email"
    "opencloud/phdenzel/password"
    "opencloud/ldenzel/email"
    "opencloud/ldenzel/password"
    "opencloud/rdenzel/email"
    "opencloud/rdenzel/password"
  ];
}
