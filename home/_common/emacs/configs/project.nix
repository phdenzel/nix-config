{...}: {
  programs.emacs.init.usePackage = {
    magit = {
      enable = true;
    };
    forge = {
      enable = true;
      after = ["magit"];
    };
    projectile = {
      enable = true;
      init = ''
        (projectile-mode +1)
      '';
      custom = {
        projectile-completion-system = "'ivy";
        projectile-indexing-method = "'alien";
      };
    };
    counsel-projectile = {
      enable = true;
      config = "(counsel-projectile-mode)";
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
  };

  home.file = {
    # Projectile bookmarks
    ".emacs.d/projectile-bookmarks.eld".text = ''
      ("~/nix-config/" "~/org" "~/zettelkasten/" "~/phdenzel.github.io/" "~/slides/" "~/Documents/letters/" "~/Documents/PhDCV/" "~/lens-forge" "~/chuchichaestli/" "~/skais-mapper/" "~/skais-model/")
    '';
  };
}
