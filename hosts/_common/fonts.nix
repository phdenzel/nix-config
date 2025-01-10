{pkgs, ...}: {
  fonts.packages = with pkgs; [
    dejavu_fonts
    hubot-sans
    mona-sans
    nerdfonts
    noto-fonts
    open-sans
    roboto
    ubuntu-sans
  ];
}
