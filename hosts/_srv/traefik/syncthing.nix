{config, ...}: {
  services.traefik.dynamicConfigOptions.http = {
    routers.syncthing = {
      rule = "Host(`syncthing.${config.networking.hostName}.home`)";
      service = "syncthing";
      entryPoints = ["web"];
    };
    services.syncthing.loadBalancer.servers = [
      {
        url = "http://127.0.0.1:8384";
      }
    ];
  };
}
