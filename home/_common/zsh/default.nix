{pkgs, config, lib, ...}: {
  programs.zsh = let
    inherit (lib) mkIf;
    packageNames = map (p: p.pname or p.name or null) config.home.packages;
    hasPackage = name: lib.any (x: x == name) packageNames;
    hasEza = hasPackage "eza";
    hasRg = hasPackage "ripgrep";
  in {
    enable = true;

    shellAliases = rec {
      ls = mkIf hasEza "eza";
      exa = ls;
      sl = "ls";
      ll = "ls -halF";
      la = "ls -a";
      l = "ls -F";
      grep = mkIf hasRg "rg";
      
      sj = "squeue -u $(whoami)";
      sja = "squeue";
      ipecho = "curl https://ipecho.net/plain; echo";
      localip = "ip -4 addr | grep inet | awk '{print $2}'";
      urlenc = "python3 -c \"import sys, urllib.parse; print(urllib.parse.quote(sys.argv[1]));\"";
      emptytrash = "rm -rf \${HOME}/.local/share/Trash/{files,info}/*";
      
    };

    autosuggestion = {
      enable = true;
      highlight = "fg=6";
    };
    syntaxHighlighting = {
      enable = true;
      package = pkgs.zsh-syntax-highlighting;
      highlighters = ["main" "brackets" "regexp"];
      styles = {
        default = "fg=15";
        unknown-token = "fg=default";
        comment = "fg=244";
        alias = "fg=10";
      };
    };
    historySubstringSearch = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    zsh-autosuggestions
    zsh-completions
    zsh-syntax-highlighting
    zsh-history-substring-search
  ];
}
