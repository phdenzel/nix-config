{...}: {
  programs.emacs.init = {
    enable = true;
    recommendedGcSettings = true;
    prelude = ''
      (setq user-full-name "Philipp Denzel")
      (setq user-main-address "phdenzel@gmail.com")

      ;; General
      (setq inhibit-startup-message t)
      (fset 'yes-or-no-p 'y-or-n-p)
			;; for better performance (in LSP) 4K -> 1M
      (setq read-process-output-max (* 1024 1024))

      ;; Buffers
			(setq Buffer-menu-use-frame-buffer-list nil)
			(setq uniquify-buffer-name-style 'forward)

      ;; Window design
			(global-tab-line-mode t)
      (global-hl-line-mode t)
			(tool-bar-mode -1)
			(scroll-bar-mode -1)
			(menu-bar-mode -1)

			;; Parentheses
      (show-paren-mode t)

			;; Clipboard
			(setq select-enable-clipboard t
			      select-enable-primary t
      			save-interprogram-paste-before-kill t
						require-final-newline t)

			;; Mouse
			(xterm-mouse-mode t)
			(mouse-wheel-mode t)
    '';
  };
}
