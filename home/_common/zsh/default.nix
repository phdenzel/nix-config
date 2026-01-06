{
  pkgs,
  config,
  ...
}: {
  imports = [
    ./autosuggestions.nix
    ./syntax-highlighting.nix
    ./history-substring-search.nix
  ];

  programs.zsh = {
    enable = true;
    dotDir = "${config.xdg.configHome}/zsh";

    initContent = ''
      fastfetch
    '';

    history = rec {
      ignoreAllDups = false;
      ignoreDups = true;
      ignoreSpace = true;
      share = true;
      size = 65536;
      save = size;
    };
  };

  home.packages = with pkgs; [
    zsh-completions
  ];

  programs.nix-your-shell = {
    enable = true;
  };
}
