{config, ...}: {
  imports = [
    ./aliases.nix
    ./ignores.nix
    # ./colors.nix
  ];

  programs.git = {
    enable = true;
    userName = "${config.home.username}";
    userEmail = "${config.home.username}@gmail.com";
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "emacsclient -c -nw";
    };
    delta.enable = true;
    delta.options = {
      true-color = "always";
      dark = true;
      syntax-theme = "${config.colorScheme.slug}";
      minus-style = "reverse bold red dim";
      minus-emph-style = "reverse bold brightred dim";
      plus-style = "reverse bold green dim";
      plus-emph-style = "reverse bold brightgreen dim";
      hunk-header-decoration-style = "blue box";
      hunk-header-style = "line-number syntax";
      line-numbers-minus-style = "brightred";
      line-numbers-plus-style = "brightgreen";
    };
  };
}
