{
  lib,
  system,
  ...
}: {
  imports =
    [
      # Home setup
      ./home.nix
      ./_configs/ssh
      ./_configs/password-store
      ../_common/xdg
      ../_common/stylix/iridis.nix
      ./_configs/repos
      ./_configs/applications
      ./_configs/pypirc

      # Program configs
      ../_common/aider-chat
      ../_common/alacritty
      ../_common/bash
      ../_common/bat
      ../_common/brave
      ../_common/btop
      # ../_common/bun
      # ../_common/dconf
      ../_common/dircolors
      # ../_common/editorconfig
      ../_common/emacs # TODO mailing
      ../_common/eza
      ../_common/fastfetch
      ../_common/firefox
      # ../_common/fzf
      # ../_common/gh
      ../_common/ghostty
      ../_common/git
      # ../_common/imv
      # ../_common/matplotlib
      # ../_common/ncmpcpp # mpd
      # ../_common/neovim
      # ./_configs/newsboat
      # ../_common/nextcloud
      # ../_common/qutebrowser
      ../_common/ruff
      ../_common/starship
      # ../_common/tealdeer
      ../_common/thunderbird
      ../_common/tmux
      ../_common/yazi
      ../_common/zsh
    ]
    # Linux-only home modules
    ++ lib.optionals (lib.hasSuffix "linux" system) [
      ./_configs/mail
      ../_common/gnome
      ../_common/gtk
      ../_common/hyprland
      ../_common/kde
      ../_common/qt
      ../_common/rofi
      ../_common/stylix
      ../_common/syncthing
    ];
}
