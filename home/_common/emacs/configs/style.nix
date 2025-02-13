{...}: {
  emacs.custom-theme.enable = true;
  programs.emacs.init.usePackage = {
    highlight-parentheses = {
      enable = false;
      config = "(global-highlight-parentheses-mode)";
    };
    rainbow-delimiters = {
      enable = true;
      hook = [
        "(prog-mode . rainbow-delimiters-mode)"
      ];
    };
    rainbow-mode = {
      enable = true;
      hook = [
        "(prog-mode . rainbow-mode)"
      ];
    };
    all-the-icons = {
      enable = true;
      custom = {
        all-the-icons-scale-factor = 0.8;
      };
    };
    all-the-icons-ibuffer = {
      enable = true;
      hook = ["(ibuffer-mode . all-the-icons-ibuffer-mode)"];
    };
    all-the-icons-ivy = {
      enable = true;
      init = ''(add-hook 'after-init-hook 'all-the-icons-ivy-setup)'';
      custom = {
        all-the-icons-spacer = "\" \"";
      };
    };
    all-the-icons-dired = {
      enable = true;
      hook = ["(dired-mode . all-the-icons-dired-mode)"];
    };
    all-the-icons-nerd-fonts = {
      enable = true;
      after = ["all-the-icons"];
      config = "(all-the-icons-nerd-fonts-prefer)";
    };
    treemacs-all-the-icons = {
      enable = true;
      after = ["treemacs" "all-the-icons"];
      config = ''(treemacs-load-theme "all-the-icons")'';
    };
    pdf-tools = {
      config = ''
        (let ((pdf-fg (phd-ark/get-color 'text))
              (pdf-bg (phd-ark/get-color 'base)))
          (setq pdf-view-midnight-colors (cons pdf-fg pdf-bg)))
      '';
    };
    dashboard = {
      enable = true;
      config = "(dashboard-setup-startup-hook)";
      custom = {
        dashboard-center-content = true;
        dashboard-set-heading-icons = true;
        dashboard-set-file-icons = true;
        dashboard-display-icons-p = true;
        dashboard-projects-backend = "'projectile";
        dashboard-projects-switch-function = "'counsel-projectile-switch-project-by-name";
        dashboard-icon-type = "'nerd-icons";
        dashboard-items = "'((projects . 5) (recents . 3))";
        dashboard-startup-banner = "\"~/.emacs.d/banners/phd-ark-128.png\"";
        dashboard-navigator-buttons = let
          init-el = "~/.emacs.d/init.el";
          gh-url = "https://www.github.com/phdenzel";
          li-url = "https://www.linkedin.com/in/philipp-denzel-7ab933142/";
        in ''
          `(
            ((,(all-the-icons-nerd-oct "mark-github" :height 1.2 :v-adjust 0.0) nil "Homepage" (lambda (&rest _) (browse-url "${gh-url}")) button)
	           (,(all-the-icons-nerd-fa "refresh" :height 1.2 :v-adjust 0.0) nil "Reload Emacs" (lambda (&rest _) (load-file "${init-el}")) font-lock-variable-name-face)
             (,(all-the-icons-nerd-md "help-circle-outline" :height 1.2 :v-adjust 0.0) nil "Show Help" (lambda (&rest _) (help-for-help)) minibuffer-prompt))
            ((,(all-the-icons-nerd-fa "linkedin" :height 1.2 :v-adjust 0.0) nil "Linkedin" (lambda (&rest _) (browse-url "${li-url}")) button)
	           (,(all-the-icons-nerd-md "security" :height 1.2 :v-adjust 0.0) nil "Pass" (lambda (&rest _) (pass)) font-lock-keyword-face))
          )
        '';
        dashboard-footer-messages = "'(\"Deus Ex Machina!\")";
        dashboard-startupify-list = let
          nl = "dashboard-insert-newline";
        in ''
          '(dashboard-insert-banner ${nl}
            dashboard-insert-navigator ${nl}
            dashboard-insert-init-info ${nl}
            dashboard-insert-items ${nl}
            dashboard-insert-footer)
        '';
      };
    };
    phd-ark-theme = {
      enable = true;
      init = ''
        (load-theme 'phd-ark :no-confirm)'';
      config = ''
        ;; Dark is already default
        ;; (setq phd-ark-polarity 'dark)
        ;; (phd-ark/reload)
        ;; Fonts
        (defvar phd-ark/mono-font "JetBrainsMono Nerd Font")
        (defvar phd-ark/pitched-font "Noto Sans")
        (when (member phd-ark/mono-font (font-family-list))
          (set-face-attribute 'default nil :font (concat phd-ark/mono-font "-12"))
          (set-face-attribute 'fixed-pitch nil :family (concat phd-ark/mono-font "-12")))
        (when (member phd-ark/pitched-font (font-family-list))
          (set-face-attribute 'variable-pitch nil :family (concat phd-ark/pitched-font "-12")))
      '';
    };
    phd-ark-tabline = {
      enable = true;
      config = "(phd-ark-tabline-mode)";
    };
    phd-ark-modeline = {
      enable = true;
      after = ["phd-ark-theme"];
      custom = {
        phd-ark-modeline-format = ''
          (list
            '(:eval (phd-ark-modeline-padding))
            '(:eval (phd-ark-modeline-marker ""))
            '(:eval (phd-ark-modeline-whitespace))
            '(:eval (phd-ark-modeline-buffer-lock-icon))
            '(:eval (phd-ark-modeline-buffer-name))
            '(:eval (phd-ark-modeline-buffer-modified-icon))
            '(:eval (phd-ark-modeline-whitespace 2))
            '(:eval (phd-ark-modeline-buffer-position))
            '(:eval (phd-ark-modeline-media-info))
            '(:eval (phd-ark-modeline-whitespace 2))
            '(:eval (phd-ark-modeline-flycheck-status))
            '(:eval (phd-ark-modeline-whitespace 4))
            '(:eval (phd-ark-modeline-vc-icon 0 0 1))
            '(:eval (phd-ark-modeline-vc-status))
            '(:eval (phd-ark-modeline-whitespace 4))
            '(:eval (phd-ark-modeline-space-between 4))
            '(:eval (phd-ark-modeline-mode-icon))
            '(:eval (phd-ark-modeline-whitespace))
            '(:eval (phd-ark-modeline-major-mode))
            '(:eval (phd-ark-modeline-whitespace))
            '(:eval (phd-ark-modeline-padding)))
        '';
      };
      config = ''
        (phd-ark-modeline-column-mode t)
        (phd-ark-modeline-mode t)
      '';
    };
  };
  home.file = {
    ".emacs.d/banners" = {
      source = ../../../../assets/logos;
      recursive = true;
    };
  };
}
