{config, ...}: let
  userName = config.home.username;
in {
  services.syncthing = {
    enable = true;
    # passwordFile = config.sops.secrets."syncthing/${userName}/password".path;
    key = "${config.sops.secrets."syncthing/${userName}/key.pem".path}";
    cert = "${config.sops.secrets."syncthing/${userName}/cert.pem".path}";
    settings = {
      devices = {
        "phinix" = {id = "Q7GMTVX-CSOMIDN-YPW3QO7-S2FWTJD-RGHDRNO-6Z7NZBH-WHEQ4KB-RO6OXA3";};
        "asahi" = {id = "TEK3CC3-2JU5QE2-S55OP5E-PSCFF7K-IMLVPAA-ZBJWE25-4CTNUDB-J7IPIQI";};
      };
      folders = {
        "Sync" = {
          path = "~/Sync";
          devices = ["phinix" "asahi"];
        };
      };
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
