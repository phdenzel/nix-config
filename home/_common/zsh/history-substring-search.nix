{pkgs, ...}: {
  programs.zsh.historySubstringSearch = {
    enable = true;
    searchDownKey = "$terminfo[kcud1]";
    searchUpKey = "$terminfo[kcuu1]";
  };

  home.packages = with pkgs; [
    zsh-history-substring-search
  ];
}
