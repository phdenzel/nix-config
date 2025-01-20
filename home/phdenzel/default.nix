{
  imports = [
    # Home setup
    ./home.nix
    ./_configs/ssh
    # ./_configs/gpg
    ../_common/xdg
    ./_configs/repos
    ../_common/stylix

    # Program configs
    ../_common/alacritty
    # ../_common/bash
    ../_common/bat
    ../_common/btop
    # ../_common/bun
    # ../_common/dconf
    ../_common/dircolors
    # ../_common/editorconfig
    ../_common/emacs
    # ../_common/eza
    # ../_common/firefox
    # ../_common/fzf
    # ../_common/gh
    # ../_common/ghostty
    ../_common/git
    # ../_common/hyprland
    # ../_common/imv
    # ../_common/joshuto
    # ../_common/matplotlib
    # ./_configs/mbsync
    # ../_common/mpv
    # ../_common/ncmpcpp
    # ../_common/neovim
    # ./_configs/newsboat
    # ./_configs/password-store
    # ../_common/rofi
    ../_common/ruff
  ];
}
