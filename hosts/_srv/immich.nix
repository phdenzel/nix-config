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
  iPhoneDir = "from-iPhone";
  iPhonePath = username: "/data/media/Pictures/${username}/${iPhoneDir}";
  albumManifests = [
    {username = "phdenzel"; root = "/data/media/Pictures/phdenzel/iPhone-archive";}
    {username = "phdenzel"; root = "/data/media/Pictures/phdenzel/GoPro";}
    {username = "phdenzel"; root = "/data/media/Pictures/phdenzel/from-android";}
    {username = "phdenzel"; root = "/data/media/Pictures/phdenzel/misc";}
  ];
  secret = username: key: config.sops.secrets."immich/${username}/${key}".path;
  mkCredentials = username: "credentials ${secret username "email"} ${secret username "password"} ${username}";
  mkLogin = username: "login ${secret username "email"} ${secret username "password"}";
  mkProvisionUser = {username, ...}: ''
    EMAIL=$(cat ${secret username "email"} | tr '[:upper:]' '[:lower:]')
    ID_${username}=$(get /admin/users | jq -r --arg email "$EMAIL" '.[] | select(.email == $email) | .id')
    if [ -z "$ID_${username}" ]; then
      ID_${username}=$(post /admin/users "$(${mkCredentials username} | jq '. + {shouldChangePassword: false}')" | jq -r .id)
    fi
    put "/admin/users/$ID_${username}" '{"storageLabel": "${username}"}' > /dev/null
    echo "Provision user ${username}: $ID_${username}"
  '';
  mkProvisionLibrary = {username, ...}: ''
    LIB_ID=$(get /libraries | jq -r --arg owner "$ID_${username}" '.[] | select(.name == "${username}" and .ownerId == $owner) | .id')
    if [ -z "$LIB_ID" ]; then
      LIB_ID=$(post /libraries "$(jq -n --arg owner "$ID_${username}" '{ownerId: $owner, name: "${username}"}')" | jq -r .id)
    fi
    put "/libraries/$LIB_ID" "$(jq -n \
      --argjson paths '${builtins.toJSON (userPaths username ++ sharedPaths)}' \
      --argjson exclusions '${builtins.toJSON ["**/${iPhoneDir}/**"]}' \
      '{importPaths: $paths, exclusionPatterns: $exclusions}')" > /dev/null
    post "/libraries/$LIB_ID/scan" "{}" > /dev/null
    ${lib.optionalString (lib.any (m: m.username == username) albumManifests) "LIB_${username}=$LIB_ID"}
    echo "Provision library ${username}: $LIB_ID"
  '';
  mkProvisionAlbums = {username, root}: ''
    if [ -f "${root}/events.tsv" ]; then
      TOKEN=$(${mkLogin username})
      provision_albums "$LIB_${username}" "${root}"
    fi
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
    login() {
      curl -s --fail-with-body -X POST "$BASE_URL/auth/login" \
        -H "Content-Type: application/json" \
        -d "$(credentials "$1" "$2" "" | jq 'del(.name)')" | jq -r .accessToken
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
    TOKEN=$(${mkLogin adminUser.username})

    get() {
      curl -s --fail-with-body "$BASE_URL$1" -H "Authorization: Bearer $TOKEN"
    }
    post() {
      curl -s --fail-with-body -X POST "$BASE_URL$1" \
        -H "Authorization: Bearer $TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2"
    }
    put() {
      curl -s --fail-with-body -X PUT "$BASE_URL$1" \
        -H "Authorization: Bearer $TOKEN" \
        -H "Content-Type: application/json" \
        -d "$2"
    }
    library_assets() {
      local cursor="" page
      while :; do
        page=$(post /search/metadata "$(jq -n --arg lib "$1" --arg cursor "$cursor" \
          '{filter: {libraryId: {eq: $lib}}, size: 1000}
            + (if $cursor == "" then {} else {cursor: $cursor} end)')")
        jq -r '.assets.items[] | [.originalPath, .id] | @tsv' <<< "$page"
        cursor=$(jq -r '.assets.nextCursor // ""' <<< "$page")
        [ -n "$cursor" ] || break
      done
    }
    provision_albums() {
      local lib=$1 root=$2 manifest=$2/events.tsv assets expected found album name id entries
      entries='($assets | split("\n") | map(select(. != "") | split("\t") | {key: .[0], value: .[1]}) | from_entries) as $ids
        | [inputs | split("\t") | {event: .[1], id: $ids[if .[0] | startswith("/") then .[0] else $root + .[0] end]} | select(.id != null)]'
      assets=$(mktemp)
      expected=$(wc -l < "$manifest")
      for i in $(seq 1 60); do
        library_assets "$lib" > "$assets"
        found=$(jq -Rn --rawfile assets "$assets" --arg root "$root/" "$entries | length" "$manifest")
        [ "$found" -ge "$expected" ] && break
        echo "Waiting for library scan of $root... ($found/$expected, $i/60)"
        sleep 10
      done
      jq -c -Rn --rawfile assets "$assets" --arg root "$root/" "$entries
        | group_by(.event) | .[] | {albumName: .[0].event, assetIds: map(.id)}" "$manifest" \
      | while IFS= read -r album; do
          name=$(jq -r .albumName <<< "$album")
          id=$(get /albums | jq -r --arg name "$name" '.[] | select(.albumName == $name) | .id' | head -n 1)
          if [ -z "$id" ]; then
            post /albums "$album" > /dev/null
          else
            put "/albums/$id/assets" "$(jq '{ids: .assetIds}' <<< "$album")" > /dev/null
          fi
          echo "Provision album $name: $(jq '.assetIds | length' <<< "$album") assets"
        done
      echo "Albums from $manifest: $found of $expected files indexed"
      rm -f "$assets"
    }

    ${lib.concatMapStrings mkProvisionUser immichUsers}
    ${lib.concatMapStrings mkProvisionLibrary immichUsers}
    ${lib.concatMapStrings mkProvisionAlbums albumManifests}

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
      storageTemplate = {
        enabled = true;
        hashVerificationEnabled = true;
        template = "{{y}}/{{y}}-{{MM}}-{{dd}}/{{filename}}";
      };
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
    "d  ${cfg.mediaLocation}/library         0700   immich    immich    -  -"
  ] ++ map (u: "d  ${cfg.mediaLocation}/library/${u.username}  0700  immich  immich  -  -") immichUsers
  ++ map (path: "d  ${path}  02770  phdenzel  media  -  -")
    (sharedPaths ++ lib.concatMap (u: userPaths u.username ++ [(iPhonePath u.username)]) immichUsers);

  systemd.services.immich-server.serviceConfig = {
    BindPaths = map (u: "${iPhonePath u.username}:${cfg.mediaLocation}/library/${u.username}") immichUsers;
    UMask = lib.mkForce "0007";
  };

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
      TimeoutStartSec = "infinity";
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
