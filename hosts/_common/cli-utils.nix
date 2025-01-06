{pkgs, lib, ...}: with lib; {
  environment.systemPackages = with pkgs; [
    bat
    eza
    fzf
    gh
    just
    rsync
    strace
  ];
  services.emacs.enable = mkDefault true;
  programs.git.enable = mkDefault true;
  programs.less.enable = mkDefault true;
  programs.tmux.enable = mkDefault true;
  programs.vim.enable = mkDefault true;
  programs.zsh.enable = mkDefault true;
}
