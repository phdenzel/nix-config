{config, ...}: {
  services.grafana = {
    enable = true;
    settings = {
      server = {
        http_addr = "127.0.0.1";
        http_port = 3000;
        domain = "localhost";
      };
      security = {
        disable_initial_admin_creation = true;
        # cookie_secure = true;
        hide_version = true;
        admin_user = "admin";
        admin_password = "\$__file{${config.sops.secrets."grafana/admin_password".path}}";
      };
    };
  };
}
