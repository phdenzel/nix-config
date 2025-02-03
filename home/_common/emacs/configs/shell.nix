{...}: {
  programs.emacs.init.usePackage = {
    term = {
      enable = true;
      custom = {
        term-prompt-regexp = "\"^[^#$%>\n]*[#$%>] *\"";
      };
    };
    vterm = {
      enable = true;
      commands = ["vterm"];
      custom = {
        vterm-shell = "\"bash\"";
        vterm-max-scrollback = 10000;
      };
    };
  };
}
