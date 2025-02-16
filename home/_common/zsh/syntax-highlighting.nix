{pkgs, config, ...}: {
  programs.zsh.syntaxHighlighting = {
    enable = true;
    package = pkgs.zsh-syntax-highlighting;
    highlighters = ["main" "brackets" "regexp"];
    styles = with config.colorScheme.palette; {
      default = "fg=#${white}";
      unknown-token = "fg=default";
      # black/grey
      comment = "fg=#${overlay2}";

      # (green, viridis)
      alias = "fg=#${viridis}";
      global-alias = "fg=#${viridis}";
      path = "fg=#${viridis},underline";
      path_pathseparator = "fg=#${viridis},bold,underline";
      autodirectory = "fg=#${viridis},underline";
      single-quoted-argument = "fg=#${green}";
      single-quoted-argument-unclosed = "fg=#${green}";
      double-quoted-argument = "fg=#${viridis}";
      double-quoted-argument-unclosed = "fg=#${viridis}";

      # (yellow, sand)
      assign = "fg=#${yellow}";
      dollar-quoted-argument = "fg=#${yellow}";
      dollar-quoted-argument-unclosed = "fg=#${yellow}";
      dollar-double-quoted-argument = "fg=#${sand}";

      # (blue, indigo)
      function = "fg=#${blue}";
      command = "fg=#${blue}";
      hashed-command = "fg=#${blue}";
      arg0 = "fg=#${blue}";
      globbing = "fg=#${blue}";
      back-quoted-argument = "fg=#${indigo}";
      back-quoted-argument-unclosed = "fg=#${indigo}";
      back-quoted-argument-delimiter = "fg=#${indigo}";

      # (pink, magenta)
      reserved-word = "fg=#${pink}";
      redirection = "fg=#${pink}";
      single-hyphen-option = "fg=#${pink}";
      double-hyphen-option = "fg=#${pink}";

      # (teal, cyan)
      command-substitution = "fg=#${teal}";
      command-substitution-unquoted = "fg=#${teal}";
      history-expansion = "fg=#${teal}";
      arithmetic-expansion = "fg=#${teal}";
      command-substitution-quoted = "fg=#${teal}";
      builtin = "fg=#${cyan}";
      precommand = "fg=#${cyan}";

      ### Brackets (blue, cyan, green, yellow)
      cursor-matchingbracket = "fg=#${base},bold,bg=#${pink}";
      bracket-error = "fg=#${ruby}";
      bracket-level-1 = "fg=#${blue},bold";
      bracket-level-2 = "fg=#${cyan},bold";
      bracket-level-3 = "fg=#${viridis},bold";
      bracket-level-4 = "fg=#${sand},bold";

      # ### Regexp (pink)
      # ZSH_HIGHLIGHT_REGEXP+=('\brm*(\s-+\w*|\w+)' fg=#${pink})
      # ZSH_HIGHLIGHT_REGEXP+=('\bexit\b' fg=#${pink})
      # ZSH_HIGHLIGHT_REGEXP+=('\blogout\b' fg=#${pink})
      # ZSH_HIGHLIGHT_REGEXP+=('\bsudo\b' fg=#${sand})
    };
  };

  home.packages = with pkgs; [
    zsh-syntax-highlighting
  ];
}
