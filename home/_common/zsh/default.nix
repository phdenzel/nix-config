{
  pkgs,
  ...
}: {
  imports = [
    ./autosuggestions.nix
    ./syntax-highlighting.nix
    ./history-substring-search.nix
  ];

  programs.zsh = {
    enable = true;

    initExtra = ''
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
}
