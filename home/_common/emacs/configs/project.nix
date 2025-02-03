{...}: {
  programs.emacs.init.usePackage = {
    magit = {
      enable = true;
    };
    projectile = {
      enable = true;
      init = ''
        (projectile-mode +1)
      '';
      config = ''
        ;; (setq projectile-known-projects-file "~/.emacs.d/projectile-bookmarks.eld")
        (setq projectile-completion-system 'ivy)
        (setq projectile-indexing-method 'alien)'';
    };
    counsel-projectile = {
      enable = true;
      after = ["counsel"];
      config = ''(counsel-projectile-mode)'';
    };
    treemacs = {
      enable = true;
      # defer = true;
      config = ''
      (progn
        (setq treemacs-position              'left
              treemacs-width                 22
              treemacs-indentation           2
              treemacs-text-scale            -1
              treemacs-show-hidden-files     t
              treemacs-is-never-other-window t
              treemacs-no-png-images         nil)
        ;; (treemacs-resize-icons 22)
        (treemacs-follow-mode t)
      )'';
    };
    treemacs-magit = {
      enable = true;
      after = ["treemacs" "magit"];
    };
    treemacs-projectile = {
      enable = true;
      after = ["treemacs" "projectile"];
    };
    treemacs-all-the-icons = {
      enable = true;
      after = ["treemacs" "all-the-icons"];
      config = ''(treemacs-load-theme "all-the-icons")'';
    };
  };
}
