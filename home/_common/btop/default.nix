{config, lib, pkgs, ...}: {
  imports = [
    ./theme.nix
  ];
  programs.btop = {
    enable = true;
    package = pkgs.stable.btop-rocm;
    settings = {
      color_theme = lib.mkOverride 99 "${config.colorScheme.slug}";
      theme_background = lib.mkOverride 99 true;
    };
  };
}
