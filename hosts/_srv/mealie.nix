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
      # Theme colors
      THEME_LIGHT_PRIMARY = "#6666cc";
      THEME_LIGHT_ACCENT = "#3366cc";
      THEME_LIGHT_SECONDARY = "#3366cc";
      THEME_LIGHT_SUCCESS = "#00AF87";
      THEME_LIGHT_INFO = "#4DB5BD";
      THEME_LIGHT_WARNING = "#FDB760";
      THEME_LIGHT_ERROR = "#D7005F";
      THEME_DARK_PRIMARY = "#6666cc";
      THEME_DARK_ACCENT = "#3366cc";
      THEME_DARK_SECONDARY = "#3366cc";
      THEME_DARK_SUCCESS = "#00AF87";
      THEME_DARK_INFO = "#4DB5BD";
      THEME_DARK_WARNING = "#FDB760";
      THEME_DARK_ERROR = "#D7005F";
    };
  };

  # networking.firewall.allowedTCPPorts = [config.services.mealie.port];
}
