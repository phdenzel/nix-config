{
  config,
  lib,
  ...
}: let
  flags = ["--advertise-routes=192.168.178.0/24" "--accept-dns=false"];
in {
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
    authKeyFile = lib.mkDefault config.sops.secrets."tailscale/authkey".path;
    extraUpFlags = ["--login-server=https://headscale.denzels.ch"] ++ flags;
    extraSetFlags = flags;
  };
  systemd.services.tailscaled-set.wantedBy = ["multi-user.target"];
  sops-host.keys = lib.mkIf (!config.services.headscale.enable) ["tailscale/authkey"];
}
