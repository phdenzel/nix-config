{pkgs, ...}: {
  fonts.packages = with pkgs;
    [
      dejavu_fonts
      hubot-sans
      mona-sans
      noto-fonts
      open-sans
      roboto
      ubuntu-sans
    ]
    ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
}
