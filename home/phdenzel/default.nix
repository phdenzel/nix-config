{
  imports = [
    # Home setup
    ./home.nix
    ./_configs/ssh
    ./_configs/gpg
    ./_configs/password-store
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
    ../_common/emacs  # TODO mu4e
    ../_common/eza
    ../_common/fastfetch
    ../_common/firefox
    # ../_common/fzf
    # ../_common/gh
    ../_common/ghostty
    ../_common/git
    # ../_common/gtk
    # ../_common/hyprland
    # ../_common/imv
    # ../_common/matplotlib
    # ./_configs/mbsync
    # ../_common/mpv
    # ../_common/ncmpcpp # mpd
    # ../_common/neovim
    # ./_configs/newsboat
    # ../_common/nextcloud
    # ../_common/qutebrowser
    # ../_common/rofi
    ../_common/ruff
    ../_common/starship
    # ../_common/swaync
    # ../_common/tealdeer
    # ./_configs/thunderbird
    # ../_common/tmux
    # ../_common/waybar
    # ../_common/wlogout
    # ../_common/yazi
    ../_common/zsh
  ];
}
