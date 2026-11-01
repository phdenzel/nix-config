{
  config,
  pkgs,
  lib,
  ...
}: let
  cfg = config.services.immich;
  immichStore = "/data/store/immich";
  immichUsers = [
    {username = "phdenzel"; isAdmin = true;}
    {username = "ldenzel"; isAdmin = false;}
    {username = "rdenzel"; isAdmin = false;}
  ];
  adminUser = lib.findFirst (u: u.isAdmin) null immichUsers;
  sharedPaths = ["/data/media/Pictures/family-photos"];
  userPaths = username: [
    "/data/media/Pictures/${username}"
  ];
  secret = username: key: config.sops.secrets."immich/${username}/${key}".path;
  mkCredentials = username: "credentials ${secret username "email"} ${secret username "password"} ${username}";
  mkProvisionUser = {username, ...}: ''
    EMAIL=$(cat ${secret username "email"} | tr '[:upper:]' '[:lower:]')
    ID_${username}=$(get /admin/users | jq -r --arg email "$EMAIL" '.[] | select(.email == $email) | .id')
    if [ -z "$ID_${username}" ]; then
      ID_${username}=$(post /admin/users "$(${mkCredentials username} | jq '. + {shouldChangePassword: false}')" | jq -r .id)
    fi
    echo "Provision user ${username}: $ID_${username}"
  '';
  mkProvisionLibrary = {username, ...}: ''
    LIB_ID=$(get /libraries | jq -r --arg owner "$ID_${username}" '.[] | select(.name == "${username}" and .ownerId == $owner) | .id')
    if [ -z "$LIB_ID" ]; then
      LIB_ID=$(post /libraries "$(jq -n \
        --arg owner "$ID_${username}" \
        --argjson paths '${builtins.toJSON (userPaths username ++ sharedPaths)}' \
        '{ownerId: $owner, name: "${username}", importPaths: $paths, exclusionPatterns: []}')" | jq -r .id)
    fi
    post "/libraries/$LIB_ID/scan" "{}" > /dev/null
    echo "Provision library ${username}: $LIB_ID"
  '';
  provisionUsers = pkgs.writeShellScript "immich-provision-users" ''
    set -euo pipefail
    SENTINEL=/var/lib/immich/.provisioned

    if [ -f "$SENTINEL" ]; then
      echo "Immich users already provisioned, skipping."
      exit 0
    fi

    BASE_URL="http://127.0.0.1:${toString cfg.port}/api"

    credentials() {
      jq -n \
        --arg email "$(cat "$1")" \
        --arg password "$(cat "$2")" \
        --arg name "$3" \
        '{email: $email, password: $password, name: $name}'
    }

    # Wait for Immich to be ready
    for i in $(seq 1 30); do
      curl -sf "$BASE_URL/server/ping" > /dev/null 2>&1 && break
      echo "Waiting for Immich to be ready... ($i/30)"
      sleep 5
    done

    if [ "$(curl -sf "$BASE_URL/server/config" | jq -r .isInitialized)" != "true" ]; then
      curl -s --fail-with-body -X POST "$BASE_URL/auth/admin-sign-up" \
        -H "Content-Type: application/json" \
        -d "$(${mkCredentials adminUser.username})" > /dev/null
    fi
    TOKEN=$(curl -s --fail-with-body -X POST "$BASE_URL/auth/login" \
      -H "Content-Type: application/json" \
      -d "$(${mkCredentials adminUser.username} | jq 'del(.name)')" | jq -r .accessToken)

    get() {
      curl -s --fail-with-body "$BASE_URL$1" -H "Authorization: Bearer $TOKEN"
    }
    post() {
      curl -s --fail-with-body -X POST "$BASE_URL$1" \
        -H "Authorization: Bearer $TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2"
    }

    ${lib.concatMapStrings mkProvisionUser immichUsers}
    ${lib.concatMapStrings mkProvisionLibrary immichUsers}

    touch "$SENTINEL"
  '';
in {
  imports = [
    ./postgresql/immich.nix
  ];
  services.immich = {
    enable = true;
    port = 2283;
    # openFirewall = true;
    host = "127.0.0.1";
    mediaLocation = "${immichStore}/media";
    machine-learning.enable = false;  # for now
    accelerationDevices = null;  # null gives access to all devices
    settings = {
      newVersionCheck.enabled = false;
      server.externalDomain = "http://immich.home";
    };
    database = {
      enable = true;
      createDB = true;
      name = "immich";
      host = "/run/postgresql";
    };
    redis = {
      enable = true;
      port = 0; # unix socket only
    };
  };
  users.users.immich.extraGroups = ["media"];

  systemd.tmpfiles.rules = [
    "d  ${immichStore}                       0755   root      root      -  -"
    "d  ${cfg.mediaLocation}                 0700   immich    immich    -  -"
    "d  ${immichStore}/dump                  0750   postgres  postgres  -  -"
  ] ++ map (path: "d  ${path}  02770  phdenzel  media  -  -")
    (sharedPaths ++ lib.concatMap (u: userPaths u.username) immichUsers);

  systemd.services.immich-provision = {
    description = "Provision initial Immich users and external libraries";
    after = ["immich-server.service"];
    wants = ["immich-server.service"];
    wantedBy = ["multi-user.target"];
    path = [pkgs.curl pkgs.jq];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = provisionUsers;
    };
  };

  sops-host.keys = lib.concatMap (u: [
    "immich/${u.username}/email"
    "immich/${u.username}/password"
  ]) immichUsers;

  # Cache
  services.redis.servers.immich.settings = {
    maxmemory = "1gb";
    maxmemory-policy = "allkeys-lru";
  };

  # Nightly pg dump
  systemd.services.immich-db-dump = {
    description = "Dump Immich PostgreSQL database";
    after = ["postgresql.service"];
    requires = ["postgresql.service"];
    serviceConfig = {
      Type = "oneshot";
      User = "postgres";
      Group = "postgres";
      ExecStart = pkgs.writeShellScript "immich-db-dump" ''
        set -euo pipefail
        DUMP_DIR="${immichStore}/dump"
        TIMESTAMP=$(date +%Y%m%d-%H%M%S)
        DUMP_FILE="$DUMP_DIR/immich-$TIMESTAMP.pgdump"

        ${config.services.postgresql.package}/bin/pg_dump \
          --format=custom \
          --compress=6 \
          "${cfg.database.name}" > "$DUMP_FILE"

        # Keep only the 7 most recent dumps
        ls -1t "$DUMP_DIR"/immich-*.pgdump 2>/dev/null \
          | tail -n +8 \
          | xargs -r rm -f

        echo "Dump complete: $DUMP_FILE"
      '';
    };
  };

  systemd.timers.immich-db-dump = {
    description = "Nightly Immich database dump";
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*-*-* 02:00:00";
      Persistent = true;
      RandomizedDelaySec = "15m";
    };
  };
}
