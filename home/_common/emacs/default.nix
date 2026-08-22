{
  pkgs,
  lib,
  ...
}: {
  programs.emacs = {
    enable = true;
    package =
      if pkgs.stdenv.hostPlatform.isDarwin
      then pkgs.emacs-macport
      else pkgs.emacs-pgtk;
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
