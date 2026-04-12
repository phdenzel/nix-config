{
  pkgs,
  config,
  ...
}: let
  rpcPort = 9091;
  hostName = config.networking.hostName;
  transmissionStore = "/data/store/transmission";
  cfg = config.services.transmission;
in {
  services = {
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
      openRPCPort = true;
      settings = {
        watch-dir-enabled = true;
        watch-dir = "${transmissionStore}/watchdir";
        incomplete-dir = "${transmissionStore}/.incomplete";
        download-dir = "${transmissionStore}/downloads";
        alt-speed-enabled = true;
        alt-speed-up = 10;
        alt-speed-down = 200000;
        rpc-port = rpcPort;
        rpc-bind-address = "0.0.0.0";
        rpc-whitelist = "127.0.0.1,192.168.*.*";
        rpc-host-whitelist-enabled = true;
        rpc-host-whitelist = "${hostName}.home,${hostName}.home:${toString rpcPort},${hostName}.local,${hostName}.local:${toString rpcPort},${hostName}";
      };
    };
  };

  systemd.tmpfiles.rules = [
    "d ${transmissionStore}/watchdir     02770 ${cfg.user} ${cfg.group} - -"
    "d ${transmissionStore}/.incomplete  02770 ${cfg.user} ${cfg.group} - -"
    "d ${transmissionStore}/downloads    02770 ${cfg.user} ${cfg.group} - -"
  ];
}
