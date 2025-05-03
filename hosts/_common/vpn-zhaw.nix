{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    openconnect
    networkmanager-openconnect
  ];

  networking.openconnect.interfaces.zhaw = {
    autoStart = false;
    protocol = "anyconnect";
    gateway = "ras.zhaw.ch";
    user = "denp@zhaw.ch";
  };
}
