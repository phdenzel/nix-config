{config, ...}: {
  imports = [
    ./aliases.nix
    ./ignores.nix
  ];
  
  programs.git = {
    enable = true;
    userName = "${config.home.username}";
    userEmail = "${config.home.username}@gmail.com";
    delta.enable = true;
    # delta.options = {};
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "emacs";
    };
  };
}
