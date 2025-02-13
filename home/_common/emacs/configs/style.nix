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
    # dashboard = {
    #
    # };
    phd-ark-theme = {
      enable = true;
      init = ''
        (load-theme 'phd-ark :no-confirm)'';
      config = ''
        ;; Dark is already default
        ;; (setq phd-ark-polarity 'dark)
        ;; (phd-ark/reload)
        ;; Fonts
        (defvar phd/mono-font "JetBrainsMono Nerd Font")
        (defvar phd/pitched-font "Noto Sans")
        (when (member phd/mono-font (font-family-list))
          (set-face-attribute 'default nil :font (concat phd/mono-font "-12"))
          (set-face-attribute 'fixed-pitch nil :family (concat phd/mono-font "-12")))
        (when (member phd/pitched-font (font-family-list))
          (set-face-attribute 'variable-pitch nil :family (concat phd/pitched-font "-12")))
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
            '(:eval (phd-ark-modeline-whitespace))
            '(:eval (phd-ark-modeline-buffer-position))
            '(:eval (phd-ark-modeline-media-info))
            '(:eval (phd-ark-modeline-whitespace))
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
}
