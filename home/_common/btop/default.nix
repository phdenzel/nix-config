{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    ./theme.nix
  ];
  programs.btop = {
    enable = true;
    # btop-rocm is Linux-only (ROCm); plain btop on darwin
    package =
      if pkgs.stdenv.hostPlatform.isDarwin
      then pkgs.btop
      else pkgs.stable.btop-rocm;
    settings = {
      color_theme = lib.mkOverride 99 "${config.colorScheme.slug}";
      theme_background = lib.mkOverride 99 true;
    };
  };
}
