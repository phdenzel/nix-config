{pkgs, config, lib, ...}: {
  imports = [
    ./autosuggestions.nix
    ./syntax-highlighting.nix
    ./history-substring-search.nix
  ];

  programs.zsh = let
    inherit (lib) mkIf;
    packageNames = map (p: p.pname or p.name or null) config.home.packages;
    hasPackage = name: lib.any (x: x == name) packageNames;
    hasEza = hasPackage "eza";
    hasRg = hasPackage "ripgrep";
  in {
    enable = true;
    
    history = rec {
      ignoreAllDups = false;
      ignoreDups = true;
      ignoreSpace = true;
      share = true;
      size = 65536;
      save = size;
    };

    shellAliases = rec {
      ls = mkIf hasEza "eza";
      exa = ls;
      sl = "ls";
      ll = "ls -halF";
      la = "ls -a";
      l = "ls -F";
      grep = mkIf hasRg "rg";
      e = "emacsclient -c -nw";
      eg = "emacsclient -c";
      se = "SUDO_EDITOR=\"emacsclient -c -nw\" sudoedit";
      seg = "SUDO_EDITOR=\"emacsclient -c\" sudoedit";
      sj = "squeue -u $(whoami)";
      sja = "squeue";
      ipecho = "curl https://ipecho.net/plain; echo";
      localip = "ip -4 addr | grep inet | awk '{print $2}'";
      urlenc = "python3 -c \"import sys, urllib.parse; print(urllib.parse.quote(sys.argv[1]));\"";
      emptytrash = "rm -rf \${HOME}/.local/share/Trash/{files,info}/*";
    };
  };

  home.packages = with pkgs; [
    zsh-completions
  ];
}
