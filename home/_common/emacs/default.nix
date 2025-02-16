{...}: {
  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    client.enable = true;
    defaultEditor = true;
    socketActivation.enable = true;
    startWithUserSession = "graphical";
  };

  imports = [
    ./epkgs.nix
    ./theme.nix
    ./configs
  ];
}
