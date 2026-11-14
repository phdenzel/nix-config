{
  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "server";
    extraSetFlags = ["--advertise-routes=192.168.178.0/24"];
  };
}
