{
  pkgs,
  lib,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs; [
    bat
    coreutils-full
    dust
    eza
    fzf
    gh
    gnutls
    gzip
    just
    jq
    libarchive
    libisoburn
    p7zip
    perl
    psmisc
    ripgrep
    rsync
    starship
    strace
    squashfsTools
    tcpdump
    tealdeer
    timer
    tree
    unzip
    xdg-utils
    yazi
    yq-go
    zip
    zoxide
    zsh
    zsh-autosuggestions
    zsh-completions
    zsh-history-substring-search
    zsh-syntax-highlighting
  ];
  programs.git.enable = mkDefault true;
  programs.less.enable = mkDefault true;
  programs.tmux.enable = mkDefault true;
  programs.vim.enable = mkDefault true;
  programs.zsh = {
    enable = mkDefault true;
    autosuggestions.enable = mkDefault true;
    enableCompletion = mkDefault true;
    syntaxHighlighting.enable = mkDefault true;
  };
}
