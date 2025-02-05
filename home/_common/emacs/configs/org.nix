{...}: {
  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      custom = {
        org-src-fontify-natively = true;
        org-src-tab-acts-natively = true;
      };
      config = ''
        (org-babel-do-load-languages
          'org-babel-load-languages
          '((emacs-lisp . t)
            (shell      . t)
            (makefile   . t)
            (python     . t)
            (C          . t)
            (js         . t)
            (dot        . t)
            (gnuplot    . t)))
      '';
    };
    org-bullets = {
      enable = true;
      after = ["org"];
      commands = ["org-bullets-mode"];
      hook = ["(org-mode . org-bullets-mode)"];
    };
  };
}
