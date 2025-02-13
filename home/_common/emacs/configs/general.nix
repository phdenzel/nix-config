{...}: {
  programs.emacs.init = {
    enable = true;
    # startupTimer = true;
    recommendedGcSettings = true;
    earlyInit = ''
      ;; Settings for startup performance
      (set-language-environment "UTF-8")
      (prefer-coding-system 'utf-8)
      (set-default-coding-systems 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)
      (setq locale-coding-system 'utf-8)
      ;; for better performance (in LSP) 4K -> 1M
      (setq read-process-output-max (* 1024 1024))

      ;; load newer compiled files
      (setq load-prefer-newer t)

      ;; Disabling some UI elements during early-init.
      (setq default-frame-alist
            '((vertical-scroll-bars . nil)
              (menu-bar-lines       . 0)
              (tool-bar-lines       . 0)))

      ;; Disable startup messages
      (advice-add #'display-startup-echo-area-message :override #'ignore)
      (advice-add #'display-startup-screen :override #'ignore)
      (setq inhibit-splash-screen t)

      ;; scratch in fundamental mode for added performance
      (setq initial-major-mode 'fundamental-mode
            initial-scratch-message nil)

      ;; Fix: unfortunately this causes errors with LSP on nix
      ;; (setenv "LSP_USE_PLISTS" "true")
    '';
    prelude =
      /*
      elisp
      */
      ''
        (setq user-full-name "Philipp Denzel")
        (setq user-mail-address "phdenzel@gmail.com")

        ;; General
        (setq user-emacs-directory "~/.emacs.d/")
        (prefer-coding-system       'utf-8)
        (set-default-coding-systems 'utf-8)
        (set-terminal-coding-system 'utf-8)
        (fset 'yes-or-no-p 'y-or-n-p)
        (setq text-scale-mode-step 1.1)

        ;; Buffers
        (setq Buffer-menu-use-frame-buffer-list nil
              uniquify-buffer-name-style 'forward)

        ;; Cursor & Scrolling
        (setq line-move-visual nil
              scroll-step 1
              scroll-conservatively 5
              pixel-scroll-precision-mode t)

        ;; Interface
        (global-tab-line-mode t)
        (global-hl-line-mode t)
        (tool-bar-mode -1)
        (scroll-bar-mode -1)
        (menu-bar-mode -1)
        ;;(defun phd/display-linum-hook ()
        ;;       (display-line-numbers-mode 1)
        ;;)
        ;;(add-hook 'prog-mode-hook 'phd/display-linum-hook)

        ;; Parentheses
        (show-paren-mode t)

        ;; Clipboard
        (transient-mark-mode 1)
        (setq select-enable-clipboard t
              select-enable-primary t
              save-interprogram-paste-before-kill t
              require-final-newline t
              mouse-yank-at-point t)

        ;; Mouse
        (xterm-mouse-mode t)
        (mouse-wheel-mode t)
        (setq mouse-wheel-progressive-speed nil
              mouse-wheel-scroll-amount '(1
                                          ((shift) . 3) ((meta) . 6)
                                          ((control) . global-text-scale)
                                          ((control meta) . text-scale)))

        ;; Backup configuration
        (defvar phd/backup-directory
          (file-name-concat user-emacs-directory "backups")
          "The subdirectory path where autosaves are stored.")
        (if (not (file-exists-p phd/backup-directory))
            (make-directory phd/backup-directory t))
        (setq backup-directory-alist `((".*" . ,phd/backup-directory))
              auto-save-file-name-transforms `((".*" ,phd/backup-directory t)))
        ;; Backup behaviour
        (setq backup-by-copying t
              version-control t
              delete-old-versions t
              kept-old-versions 5
              kept-new-versions 10)
        ;; Backup cleanup
        (setq delete-by-moving-to-trash t
              trash-directory "~/.local/share/Trash/files")
      '';
  };
}
