{pkgs, config, ...}: let
  rpcPort = 9091;
  hostName = config.networking.hostName;
in {
  services = {
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
      openRPCPort = true;
      settings = {
        alt-speed-enabled = true;
        alt-speed-up = 10;
        alt-speed-down = 200000;
        rpc-port = rpcPort;
        rpc-bind-address = "0.0.0.0";
        rpc-whitelist    = "127.0.0.1,192.168.*.*";
        rpc-host-whitelist-enabled = true;
        rpc-host-whitelist = "${hostName}.home,${hostName}.home:${toString rpcPort},${hostName}.local,${hostName}.local:${toString rpcPort},${hostName}";
      };
    };
  };
}
