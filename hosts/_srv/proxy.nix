{config, ...}: let
  hosts = {
    ygdrasil = "127.0.0.1";
    heimdall = "192.168.178.64";
    phinix = "192.168.178.156";
  };
  mkGlancesRouter = name: {
    rule = "Host(`glances.${name}.home`)";
    service = "glances-${name}";
    entryPoints = ["web"];
  };
  mkGlancesService = ip: {
    loadBalancer.servers = [{url = "http://${ip}:${toString config.services.glances.port}";}];
  };
in {
  services.traefik = {
    enable = true;

    staticConfigOptions = {
      entryPoints.web.address = ":80";
      api.dashboard = true; # traefik dashboard on port 8080 -> http://traefik.home
      providers.file.filename = "/etc/traefik/dynamic.json";
      providers.file.watch = true;
    };

    dynamicConfigOptions = {
      http = {
        routers = {
          dashboard = {
            rule = "Host(`denzels.home`)";
            service = "homepage-dashboard";
            entryPoints = ["web"];
          };
          traefik = {
            rule = "Host(`traefik.home`)";
            service = "api@internal";
            entryPoints = [ "web" ];
          };
          forgejo = {
            rule = "Host(`forgejo.home`)";
            service = "forgejo";
            entryPoints = ["web"];
          };
          jellyfin = {
            rule = "Host(`jellyfin.home`)";
            service = "jellyfin";
            entryPoints = ["web"];
          };
          transmission = {
            rule = "Host(`transmission.home`)";
            service = "transmission";
            entryPoints = ["web"];
          };
          vikunja = {
            rule = "Host(`vikunja.home`)";
            service = "vikunja";
            entryPoints = ["web"];
          };
          mealie = {
            rule = "Host(`mealie.home`)";
            service = "mealie";
            entryPoints = ["web"];
          };
          glances-ygdrasil = mkGlancesRouter "ygdrasil";
          glances-heimdall = mkGlancesRouter "heimdall";
          glances-phinix = mkGlancesRouter "phinix";
        };

        services = {
          dashboard.loadBalancer.servers = [{url = "http://127.0.0.1:${toString config.services.homepage-dashboard.listenPort}";}];
          forgejo.loadBalancer.servers = [{url = "http://127.0.0.1:${toString config.services.forgejo.settings.server.HTTP_PORT}";}];
          jellyfin.loadBalancer.servers = [{url = "http://127.0.0.1:8096";}];
          transmission.loadBalancer.servers = [{url = "http://127.0.0.1:${toString config.services.transmission.settings.rpc-port}";}];
          vikunja.loadBalancer.servers = [{url = "http://127.0.0.1:${toString config.services.vikunja.port}";}];
          mealie.loadBalancer.servers = [{url = "http://127.0.0.1:${toString config.services.mealie.port}";}];
          glances-ygdrasil = mkGlancesService hosts.ygdrasil;
          glances-heimdall = mkGlancesService hosts.heimdall;
          glances-phinix   = mkGlancesService hosts.phinix;
        };
      };
    };
  };

  networking.firewall.allowedTCPPorts = [80];
}
