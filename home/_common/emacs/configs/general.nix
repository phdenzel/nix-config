{...}: {
  programs.emacs.init = {
    enable = true;
    # earlyInit = ''
    #   (setenv "LSP_USE_PLISTS" "true")
    # '';
    recommendedGcSettings = true;
    prelude = ''
         (setq user-full-name "Philipp Denzel")
         (setq user-main-address "phdenzel@gmail.com")

         ;; General
         (setq inhibit-startup-message t)
         (fset 'yes-or-no-p 'y-or-n-p)
      ;; for better performance (in LSP) 4K -> 1M
         (setq read-process-output-max (* 1024 1024))
         (prefer-coding-system 'utf-8)

         ;; Buffers
         (setq Buffer-menu-use-frame-buffer-list nil
            uniquify-buffer-name-style 'forward)

         ;; Cursor
         (setq line-move-visual nil
               scroll-step 1
               scroll-conservatively 5)

         ;; Interface
      (global-tab-line-mode t)
         (global-hl-line-mode t)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (menu-bar-mode -1)
         (defun phd/display-linum-hook ()
                (display-line-numbers-mode 1)
         )
         (add-hook 'prog-mode-hook 'phd/display-linum-hook)

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
    '';
  };
}
