{config, ...}: let
  userName = config.home.username;
in {
  services.syncthing = {
    enable = true;
    # passwordFile = config.sops.secrets."syncthing/${userName}/password".path;
    key = "${config.sops.secrets."syncthing/${userName}/key.pem".path}";
    cert = "${config.sops.secrets."syncthing/${userName}/cert.pem".path}";
    settings = {
      options = {
        relaysEnabled = false;
        urAccepted = -1;
      };
    };
    tray = {
      enable = true;
    };
  };
}
