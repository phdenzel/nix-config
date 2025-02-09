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
  };
}
