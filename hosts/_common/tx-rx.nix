{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    cyrus-sasl-xoauth2
    filezilla
    iputils
    pizauth
    wget
  ];
  services = {
    transmission = {
      enable = true;
      package = pkgs.transmission_4;
      settings = {
        alt-speed-enabled = true;
        alt-speed-up = 10;
        alt-speed-down = 200000;
      };
    };
  };
}
