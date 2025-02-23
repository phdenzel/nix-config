{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    transmission_4
    transmission_4-gtk
    cyrus-sasl-xoauth2
    pizauth
  ];
  services = {
    # tailscale.enable = true;
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
    };
  };
}
