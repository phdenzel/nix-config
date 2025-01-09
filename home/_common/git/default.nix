{config, ...}: {
  imports = [
    ./aliases.nix
    ./ignores.nix
  ];
  
  programs.git = {
    enable = true;
    userName = "${config.userName}";
    userEmail = "${config.userName}@gmail.com";
    delta.enable = true;
    # delta.options = {};
    extraConfig = {
      init.defaultBranch = "main";
      core.editor = "emacs";
    };
  };
}
