{...}: {
  programs.emacs.init = {
    globalBinds = {
      "<f5>" = "revert-buffer";
      "C-x k" = "kill-this-buffer";
      "C-a" = "back-to-indentation";
      "<M-up>" = "backward-paragraph";
      "<M-down>" = "forward-paragraph";
      "C-M-'" = "other-window";
      "C-M-S-h" = "shrink-window-horizontally";
      "C-M-S-l" = "enlarge-window-horizontally";
      "C-M-S-j" = "enlarge-window";
      "C-M-S-k" = "shrink-window";
    };
    usePackage = {
      # Navigation
      dired.bindLocal.dired-mode-map = {
        "<return>" = "dired-find-alternate-file";
        "r" = "dired-do-rename-regexp";
      };
      counsel.bind = {
        "M-x" = "counsel-M-x";
        "C-x b" = "counsel-switch-buffer";
        "C-x C-b" = "ibuffer";
        "C-x C-f" = "counsel-find-file";
        "C-x C-y" = "counsel-yank-pop";
        "C-c i u" = "counsel-unicode-char";

        "M-i" = "counsel-imenu";
      };
      swiper.bind = {
        "C-s" = "swiper-isearch";
      };
      avy.bind = {
        "M-s" = "avy-goto-word-1";
      };
      ace-window.bind = {
        "C-x o" = "ace-window";
      };
      # Editing
      multiple-cursors.bind = {
        "M-SPC" = "set-rectangular-regin-anchor";
        "C-c ," = "mc/edit-lines";
        "C->" = "mc/mark-next-like-this";
        "C-<" = "mc/mark-previous-like-this";
        "C-c C-<" = "mc/mark-all-like-this";
        "<C-S-mouse-1>" = "mc/add-cursor-on-click";
      };
      sudo-edit.bind = {
        "C-c C-s C-e" = "sudo-edit";
      };
      comment-dwim-2.bind = {
        "M-/" = "comment-dwim-2";
      };
      hungry-delete.bind = {
        "C-<backspace>" = "hungry-delete-backward";
        "C-M-<backspace>" = "hungry-delete-backward";
        "C-M-d" = "hungry-delete-forward";
      };
      expand-region.bind = {
        "C-M-SPC" = "er/expand-region";
      };
      drag-stuff.bind = {
        "<C-M-up>" = "drag-stuff-up";
        "<C-M-down>" = "drag-stuff-down";
      };
      # Dev
    };
  };
}
