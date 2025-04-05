{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    age
    openssl
    pwgen
    ssh-to-age
    sops
    zbar
  ];

  programs.gnupg.agent = {
    enable = mkDefault true;
    enableSSHSupport = mkDefault true;
  };
}
