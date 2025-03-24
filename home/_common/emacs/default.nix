{pkgs, ...}: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacs-pgtk;
  };
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
