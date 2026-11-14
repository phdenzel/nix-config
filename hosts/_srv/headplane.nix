{config, ...}: {
  services.headplane = {
    enable = true;
    settings.server = {
      host = "127.0.0.1";
      port = 3000;
      cookie_secret_path = config.sops.secrets."headscale/headplane_cookie_secret".path;
      cookie_secure = false;
    };
  };

  services.traefik.dynamicConfigOptions.http = {
    routers.headplane = {
      rule = "Host(`headplane.home`)";
      service = "headplane";
      entryPoints = ["web"];
      middlewares = ["headplane-root"];
    };
    middlewares.headplane-root.redirectRegex = {
      regex = "^http://headplane\\.home/?$";
      replacement = "http://headplane.home/admin/";
    };
    services.headplane.loadBalancer.servers = [
      {url = "http://127.0.0.1:${toString config.services.headplane.settings.server.port}";}
    ];
  };

  sops-host.ownedKeys = ["headscale/headplane_cookie_secret"];
}
