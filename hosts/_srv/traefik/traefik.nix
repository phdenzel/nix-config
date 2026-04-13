{
  config,
  ...
}: {
  services.traefik = {
    enable = true;
    staticConfigOptions = {
      entryPoints.web.address = ":80";
      api.dashboard = true; # on port 8080
    };
    dynamicConfigOptions.http = {
      routers.hostTraefik = {
        rule = "Host(`traefik.${config.networking.hostName}.home`)";
        service = "api@internal";
        entryPoints = ["web"];
      };
    };
  };
  networking.firewall.allowedTCPPorts = [80];
}
