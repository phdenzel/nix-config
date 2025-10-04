{pkgs,  ...}: {
  services.forgejo = {
    enable = true;
    package = pkgs.forgejo;
    settings.server = {
      PROTOCOL = "http";
      SSH_PORT = 22;
      HTTP_PORT = 3022;
    };
    # database = {
    #   type = "sqlite3";
    #   port = 3306;
    # };
    lfs.enable = true;
    dump = {
      enable = true;
      type = "tar.gz";
      interval = "01:30";
    };
    
  };
}
