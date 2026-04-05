{config, ...}: {
  services.mealie = {
    enable = true;
    port = 9000;
    settings = {
      ALLOW_SIGNUP = "false";
      BASE_URL = "http://${config.networking.hostName}.home:${toString config.services.mealie.port}";
      DEFAULT_EMAIL = "admin@mealie.home";
      DEFAULT_GROUP = "Home";
      DEFAULT_HOUSEHOLD = "Family";
      TZ = "Europe/Zurich";
    };
  };
}
