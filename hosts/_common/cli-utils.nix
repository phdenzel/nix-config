{
  pkgs,
  lib,
  config,
  ...
}:
with lib; {
  environment.systemPackages = with pkgs;
    [
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
      mermaid-cli
      p7zip
      perl
      ripgrep
      rsync
      starship
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
    ]
    ++ optionals stdenv.isLinux [
      # Linux-only packages
      psmisc
      strace
    ]
    ++ optionals stdenv.isDarwin [
      # macOS alternatives
      pstree
    ];

  programs =
    {
      tmux.enable = mkDefault true;
      vim.enable = mkDefault true;
      zsh =
        {
          enable = mkDefault true;
          enableCompletion = mkDefault true;
        }
        # zsh autosuggestions/syntax-highlighting options differ
        // (
          if config.nixpkgs.hostPlatform.isDarwin
          then {
            enableAutosuggestions = mkDefault true;
            enableSyntaxHighlighting = mkDefault true;
          }
          else {
            autosuggestions.enable = mkDefault true;
            syntaxHighlighting.enable = mkDefault true;
          }
        );
    }
    # programs.git and programs.less are NixOS-only modules
    // optionalAttrs config.nixpkgs.hostPlatform.isLinux {
      git.enable = mkDefault true;
      less.enable = mkDefault true;
    };
}
