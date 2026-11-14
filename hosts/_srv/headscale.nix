{config, ...}: let
  domain = "headscale.denzels.ch";
in {
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
    };
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
