{inputs, pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    openconnect
    networkmanager-openconnect
    inputs.openconnect-sso.packages.${pkgs.system}.openconnect-sso
  ];

  networking.openconnect.interfaces.zhaw = {
    autoStart = false;
    protocol = "anyconnect";
    gateway = "ras.zhaw.ch";
    user = "denp@zhaw.ch";
  };
}
