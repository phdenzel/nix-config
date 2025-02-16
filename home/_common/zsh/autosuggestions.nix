{pkgs, config, ...}: {
  programs.zsh.autosuggestion = {
    enable = true;
    highlight = with config.colorScheme.palette; "fg=#${teal}";
  };

  home.packages = with pkgs; [
    zsh-autosuggestions
  ];
}
