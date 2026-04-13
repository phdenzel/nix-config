{config, ...}: {
  services.traefik.dynamicConfigOptions.http = {
    routers.glances = {
      rule = "Host(`glances.${config.networking.hostName}.home`)";
      service = "glances";
      entryPoints = ["web"];
    };
    services.glances.loadBalancer.servers = [
      {
        url = "http://127.0.0.1:${toString config.services.glances.port}";
      }
    ];
  };
}
