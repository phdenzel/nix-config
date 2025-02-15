{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    transmission_4
    transmission_4-gtk
  ];
  services = {
    # tailscale.enable = true;
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
    };
  };
}
