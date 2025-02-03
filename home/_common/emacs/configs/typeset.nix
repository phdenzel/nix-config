{...}: {
  programs.emacs.init.usePackage = {
    jinx = {
      enable = true;
      hook = [
        "(text-mode . jinx-mode)"
        "(LaTeX-mode . jinx-mode)"
        "(latex-mode . jinx-mode)"
        "(bibtex-mode . jinx-mode)"
        "(org-mode . jinx-mode)"
      ];
    };
    auctex = {
      enable = true;
    };
  };
}
