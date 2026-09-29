{
  pkgs,
  lib,
  ...
}: let
  emacsPackage =
    if pkgs.stdenv.hostPlatform.isDarwin
    then pkgs.emacs-macport
    else pkgs.emacs-pgtk;
in {
  programs.emacs = {
    enable = true;
    package = emacsPackage;
    # Emacs mis-parses archive names ending in `-<digits>' (bug#77143, bug#80744).
    # nixpkgs-unstable dropped the elpa2nix workaround for it; stable still has it.
    overrides = _efinal: _eprev: {
      comment-dwim-2 = (pkgs.stable.emacsPackagesFor emacsPackage).comment-dwim-2;
    };
  };
  services.emacs =
    {
      enable = true;
      client.enable = true;
      defaultEditor = true;
    }
    // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
      socketActivation.enable = true;
      startWithUserSession = "graphical";
    };

  imports = [
    ./epkgs.nix
    ./theme.nix
    ./configs
  ];
}
