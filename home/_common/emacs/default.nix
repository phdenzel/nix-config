{...}: {
  programs.emacs.enable = true;
  services.emacs.enable = true;
  services.emacs.client.enable = true;
  services.emacs.defaultEditor = true;

  imports = [
    ./epkgs.nix
    # ./overrides.nix
    ./theme.nix
    ./configs
  ];
}
