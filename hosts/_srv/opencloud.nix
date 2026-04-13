{pkgs, config, lib, ...}: let
  cfg = config.services.opencloud;
  cloudData = "/data/store/opencloud";  # PosixFS root
  # only single filesystem roots possible so bind-mount others
  extraRoots = [
    {name = "Media"; path = "/data/media";}
    {name = "Shared"; path = "/data/documents";}
    {name = "Backups"; path = "/data/backups";}
  ];
  # phdenzel is provisioned as admin via OC_ADMIN_USER_ID + INITIAL_ADMIN_PASSWORD
  opencloudUsers = [
    {username = "ldenzel"; isAdmin = false;}
    {username = "rdenzel"; isAdmin = false;}
  ];
  mkProvisionUser = {username, isAdmin}: ''
    USER_PASS=$(cat ${config.sops.secrets."opencloud/${username}/password".path})
    USER_EMAIL=$(cat ${config.sops.secrets."opencloud/${username}/email".path})
    ${pkgs.curl}/bin/curl -sf \
      -H "Authorization: Bearer $TOKEN" \
      -X POST "$BASE_URL/graph/v1.0/users" \
      -H "Content-Type: application/json" \
      -d "{
        \"onPremisesSamAccountName\": \"${username}\",
        \"displayName\": \"${username}\",
        \"mail\": \"$USER_EMAIL\",
        \"passwordProfile\": {\"password\": \"$USER_PASS\"}
      }" \
      || echo "Warning: failed to create user ${username} (may already exist)"
  '';
  provisionUsers = pkgs.writeShellScript "opencloud-provision-users" ''
    set -euo pipefail
    SENTINEL="${cfg.stateDir}/.provisioned"

    if [ -f "$SENTINEL" ]; then
      echo "OpenCloud users already provisioned, skipping."
      exit 0
    fi

    ADMIN_PASS=$(cat ${config.sops.secrets."opencloud/phdenzel/password".path})
    BASE_URL="http://127.0.0.1:${toString cfg.port}"

    # Wait for OpenCloud to be ready
    for i in $(seq 1 30); do
      ${pkgs.curl}/bin/curl -sf "$BASE_URL/health/ready" > /dev/null 2>&1 && break
      echo "Waiting for OpenCloud to be ready... ($i/30)"
      sleep 5
    done

    # Obtain an OIDC access token via the resource owner password grant.
    # The client_id below is the well-known public desktop client shipped with OpenCloud.
    # If token acquisition fails, verify the /konnect/v1/token endpoint and client_id
    # against the running version's IDP configuration.
    TOKEN=$(${pkgs.curl}/bin/curl -sf \
      "$BASE_URL/konnect/v1/token" \
      -d "grant_type=password" \
      -d "client_id=xdXOt13JKxym1B1QcEncf2XDkLAexMBFwiT9j6EfhhHFJhs2KM9jbjTmf8JBXE69" \
      -d "username=phdenzel" \
      -d "password=$ADMIN_PASS" \
      -d "scope=openid profile email" \
      | ${pkgs.jq}/bin/jq -r '.access_token')

    ${lib.concatMapStrings mkProvisionUser opencloudUsers}

    touch "$SENTINEL"
  '';
  
in {
  services.opencloud = {
    enable = true;
    port = 9200;
    address = "127.0.0.1";
    url = "http://opencloud.home";
    environment = {
      # no need for TLS with traefik reverse proxy
      PROXY_TLS = "false";
      # no TLS for internal calls
      OC_INSECURE = "true";
      # admin account
      OC_ADMIN_USER_ID = "phdenzel";
      # storage as standard POSIX file tree
      STORAGE_USERS_DRIVER = "posix";
      # file tree root
      STORAGE_USERS_POSIX_ROOT = cloudData;
      # saver if multiple services access filesystem
      STORAGE_USERS_POSIX_WATCH_TYPE = "watchfolder";
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

  # bind-mount extra roots as subdirectories
  fileSystems = lib.listToAttrs (map (r: {
    name = "${cloudData}/${r.name}";
    value = {
      device = r.path;
      fsType = "none";
      options = ["bind" "nofail"];
    };
  }) extraRoots);

  systemd.services.opencloud-provision = {
    description = "Provision initial OpenCloud users";
    after = ["opencloud.service"];
    wants = ["opencloud.service"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = provisionUsers;
    };
  };

  sops-host.keys = [
    "opencloud/env"  # INITIAL_ADMIN_PASSWORD
    "opencloud/phdenzel/email"
    "opencloud/phdenzel/password"
    "opencloud/ldenzel/email"
    "opencloud/ldenzel/password"
    "opencloud/rdenzel/email"
    "opencloud/rdenzel/password"
  ];
}
