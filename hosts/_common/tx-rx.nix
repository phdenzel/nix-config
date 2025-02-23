{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    cyrus-sasl-xoauth2
    filezilla
    pizauth
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
