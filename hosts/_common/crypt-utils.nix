{pkgs, lib, ...}: with lib; {
  environment.systemPackages = with pkgs; [
    age
    ssh-to-age
    sops
  ];

  programs.gnupg.agent = {
    enable = mkDefault true;
    enableSSHSupport = mkDefault true;
  };
}
