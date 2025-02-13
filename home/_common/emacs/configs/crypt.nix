{...}: {
  programs.emacs.init.usePackage = {
    pass = {
      enable = true;
      custom = {
        pass-username-field = "\"login\"";
      };
    };
  };
}
