{
  pkgs,
  lib,
  ...
}: {
  fonts.packages = with pkgs;
    [
      dejavu_fonts
      fira-sans
      hubot-sans
      mona-sans
      noto-fonts
      open-sans
      roboto
      ubuntu-sans
    ]
    ++ lib.optionals stdenv.hostPlatform.isLinux [
      liberation_ttf
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
}
