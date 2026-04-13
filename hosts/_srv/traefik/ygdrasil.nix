{
  config,
  lib,
  ...
}: let
  inherit (builtins) map listToAttrs;
  localServices = [
    {
      hostname = "traefik";
      service = "api@internal";
    }
    {
      hostname = "denzels";
      service = "homepage-dashboard";
      port = config.services.homepage-dashboard.listenPort;
    }
    {
      hostname = "opencloud";
      port = config.services.opencloud.port;
    }
    {
      hostname = "forgejo";
      port = config.services.forgejo.settings.server.HTTP_PORT;
    }
    {
      hostname = "jellyfin";
      port = 8096;
    }
    {
      hostname = "transmission";
      port = config.services.transmission.settings.rpc-port;
    }
    {
      hostname = "vikunja";
      port = config.services.vikunja.port;
    }
    {
      hostname = "mealie";
      port = config.services.mealie.port;
    }
    {
      hostname = "glances.heimdall";
      service = "glances-heimdall";
      ip = "192.168.178.64";
      port = 80;
    }
    {
      hostname = "glances.phinix";
      service = "glances-phinix";
      ip = "192.168.178.156";
      port = 80;
    }
  ];
  mkRouter = attrs: {
    rule = "Host(`${attrs.hostname}.home`)";
    service = attrs.service or attrs.hostname;
    entryPoints = ["web"];
  };
  mkService = attrs: {
    loadBalancer.servers = [{url = "http://${attrs.ip or "127.0.0.1"}:${toString attrs.port}";}];
  };
in {
  services.traefik.dynamicConfigOptions.http = {
    routers = listToAttrs (map (attrs: {
        name = attrs.service or attrs.hostname;
        value = mkRouter attrs;
      })
      localServices);

    services = listToAttrs (map (attrs: {
        name = attrs.service or attrs.hostname;
        value = mkService attrs;
      })
      (builtins.filter (attrs: attrs ? port) localServices));
  };
}
