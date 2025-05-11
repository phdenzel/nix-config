{...}: {
  services.syncthing = {
    enable = true;
    settings = {
      options = {
        relaysEnabled = false;
        urAccepted = -1;
      };
    };
    tray = {
      enable = true;
    };
  };

  imports = [
    ./devices.nix
    ./folders.nix
    ./ignores.nix
  ];
}
