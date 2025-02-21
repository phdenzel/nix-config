# This is a home.nix generated by the follwing hone-manager command
#
# home-manager init ./
{pkgs, ...}: let
  userName = "phdenzel";
in {
  home.username = "${userName}";
  home.homeDirectory = "/home/${userName}";
  home.stateVersion = "24.11";

  home.shell = {
    enableShellIntegration = true;
  };

  home.shellAliases = rec {
    ls = "eza";
    exa = ls;
    sl = ls;
    ll = "ls -glaF";
    la = "ls -a";
    l = "ls -F";
    grep = "rg";
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

  programs = {
    home-manager.enable = true; # let home manager install and manage itself.
    mu.enable = true; # mail indexing
  };

  # The home.packages option allows you to install Nix packages into your
  # environment.
  # home.packages = with pkgs; [];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/m3tam3re/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    BROWSER = "firefox";
    PAGER = "less";
    TERMINAL = "ghostty";
    VISUAL = "emacs";
  };
}
