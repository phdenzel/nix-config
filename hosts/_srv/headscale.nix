{
  config,
  lib,
  pkgs,
  ...
}: let
  domain = "headscale.denzels.ch";
  users = ["phdenzel"];
  homeSubnet = "192.168.178.0/24";
  localAuthKey = "/run/headscale-users/tailscale-authkey";
in {
  imports = [./headplane.nix];

  services.headscale = {
    enable = true;
    address = "127.0.0.1";
    port = 8085;
    settings = {
      server_url = "https://${domain}";
      dns = {
        magic_dns = true;
        base_domain = "tailscale.denzels.ch";
        override_local_dns = false;
        nameservers.split."home" = ["192.168.178.16"];
      };
      policy.path = pkgs.writeText "headscale-policy.json" (builtins.toJSON {
        acls = [
          {
            action = "accept";
            src = ["*"];
            dst = ["*:*"];
          }
        ];
        autoApprovers.routes.${homeSubnet} = map (user: "${user}@") users;
      });
    };
  };

  systemd.services.headscale-users = {
    description = "Ensure headscale users exist";
    after = ["headscale.service"];
    requires = ["headscale.service"];
    wantedBy = ["multi-user.target"];
    path = [config.services.headscale.package pkgs.jq];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      TimeoutStartSec = "60s";
      RuntimeDirectory = "headscale-users";
      RuntimeDirectoryMode = "0700";
    };
    script = ''
      until headscale users list -o json >/dev/null 2>&1; do sleep 1; done
      for user in ${lib.escapeShellArgs users}; do
        headscale users list -o json | jq -e --arg u "$user" 'any(.[]?; .name == $u)' >/dev/null \
          || headscale users create "$user"
      done
      id=$(headscale users list -o json | jq -r --arg u ${lib.escapeShellArg (builtins.head users)} '.[] | select(.name == $u) | .id')
      headscale preauthkeys create --user "$id" --expiration 1h -o json | jq -r .key > ${localAuthKey}
    '';
  };
  services.tailscale.authKeyFile = lib.mkForce localAuthKey;
  systemd.services.tailscaled-autoconnect = {
    after = ["headscale-users.service" "traefik.service"];
    requires = ["headscale-users.service"];
  };

  services.traefik = {
    staticConfigOptions = {
      entryPoints.public.address = ":4443";
      certificatesResolvers.infomaniak.acme = {
        email = "mail@denzels.ch";
        storage = "${config.services.traefik.dataDir}/acme.json";
        dnsChallenge.provider = "infomaniak";
      };
    };
    environmentFiles = [config.sops.secrets."traefik/env".path];
    dynamicConfigOptions.http = {
      routers.headscale = {
        rule = "Host(`${domain}`)";
        service = "headscale";
        entryPoints = ["websecure" "public"];
        tls.certResolver = "infomaniak";
      };
      services.headscale.loadBalancer.servers = [
        {url = "http://127.0.0.1:${toString config.services.headscale.port}";}
      ];
    };
  };

  networking.firewall.allowedTCPPorts = [4443];
  sops-host.keys = ["traefik/env"];
}
