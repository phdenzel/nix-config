{
  config,
  lib,
  pkgs,
  ...
}: let
  virtualIp = "192.168.178.16";
  nodes = {
    heimdall = {
      interface = "end0";
      address = "192.168.178.64";
      priority = 150;
    };
    ygdrasil = {
      interface = "enp3s0";
      address = "192.168.178.42";
      priority = 100;
    };
  };
  self = nodes.${config.networking.hostName};
  peers = lib.attrValues (lib.removeAttrs nodes [config.networking.hostName]);
  isPrimary = lib.all (peer: self.priority > peer.priority) peers;
  checkBlocky = pkgs.writeShellScript "check-blocky" ''
    [ "$(${pkgs.dnsutils}/bin/dig +time=1 +tries=1 +short @127.0.0.1 dns.home A)" = "${virtualIp}" ]
  '';
in {
  services.keepalived = {
    enable = true;
    openFirewall = true;
    enableScriptSecurity = true;
    vrrpScripts.blocky = {
      script = "${checkBlocky}";
      interval = 2;
      fall = 2;
      rise = 2;
    };
    vrrpInstances.dns = {
      inherit (self) interface priority;
      state =
        if isPrimary
        then "MASTER"
        else "BACKUP";
      virtualRouterId = 16;
      unicastSrcIp = self.address;
      unicastPeers = map (peer: peer.address) peers;
      virtualIps = [{addr = "${virtualIp}/24";}];
      trackScripts = ["blocky"];
    };
  };

  users.users.keepalived_script = {
    isSystemUser = true;
    group = "keepalived_script";
  };
  users.groups.keepalived_script = {};
}
