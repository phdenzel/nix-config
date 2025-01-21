{...}: {
  programs.emacs.init.usePackage = {
    lsp-mode = {
      enable = true;
      commands = ["lsp" "lsp-deferred"];
      hook = [
        "(python-mode . lsp)"
        "(rust-mode . lsp)"
        "(nix-mode . lsp-deferred)"
        "(c-mode   . lsp-deferred)"
        "(c++-mode . lsp-deferred)"
        "(LaTeX-mode . lsp)"
        "(latex-mode . lsp)"
        "(bibtex-mode . lsp)"
      ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
      '';
      config = ''
        (setq lsp-inline-completion-enable t
              lsp-pylsp-configuration-sources '("ruff")
              lsp-pylsp-plugins-ruff-enabled t
              lsp-pylsp-plugins-flake8-enabled nil
              lsp-pylsp-plugins-pycodestyle-enabled nil
              lsp-pylsp-rename-backend 'rope)
      '';
    };
    lsp-ivy = {
      enable = true;
      commands = ["lsp-ivy-workspace-symbol"];
    };
    lsp-ui = {
      enable = false;
      after = ["lsp-mode"];
      commands = ["lsp-ui-mode"];
      config = ''
        (setq lsp-ui-sideline-enable nil
              lsp-ui-sideline-show-symbol nil
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-update-mode 'point
              lsp-ui-doc-enable t)
      '';
    };
    lsp-treemacs = {
      enable = true;
      after = ["lsp-mode" "treemacs"];
      commands = ["lsp-treemacs-errors-list" "lsp-treemacs-symbols"];
      config = ''
        (lsp-treemacs-sync-mode 1)
        (setq lsp-treemacs-deps-position-params
              '((side . right)
                (slot . 1)
                (window-width . 35)))
        (setq lsp-treemacs-symbols-position-params
              '((side . right)
                (slot . 2)
                (window-width . 35)))
      '';
    };
    dap-mode = {
      enable = true;
      after = ["lsp-mode"];
      commands = ["dap-mode" "dap-ui-mode"];
      config = ''
        (require 'dap-python)
        (setq dap-python-debugger 'debugpy)
        (dap-mode t)
        (dap-ui-mode t)
      '';
    };
    pyvenv = {
      enable = true;
      config = ''
        (setq pyvenv-default-workon "lsp")
        (pyvenv-mode 1)
        (pyvenv-tracking-mode 1)
        (pyvenv-workon pyvenv-default-workon)
      '';
    };
    ein = {
      enable = true;
      config = ''
        (setq ein:output-area-inlined-images t)
      '';
    };

    python-mode.enable = true;
    cython-mode = {
      enable = true;
      mode = [
        ''"\\.pyx\\'"''
        ''"\\.pxd\\'"''
        ''"\\.pxi\\'"''
      ];
    };
    rust-mode = {
      enable = true;
      mode = [''"\\.rs\\'"''];
    };
    haskell-mode = {
      enable = true;
      mode = [
        ''("\\.hs\\'" . haskell-mode)''
        ''("\\.hsc\\'" . haskell-mode)''
      ];
      config = ''
        (require 'haskell)
        (require 'haskell-doc)
        (setq haskell-process-auto-import-loaded-modules t
              haskell-process-suggest-remove-import-lines t
              haskell-process-log t
              haskell-notify-p t)
        (setq haskell-process-args-cabal-repl
              '("--ghc-options=+RTS -M500m -RTS -ferror-spans -fshow-loaded-modules"))
      '';
    };
    nix-mode = {
      enable = true;
      mode = [''"\\.nix\\'"''];
    };
    just-mode.enable = true;
    dockerfile-mode.enable = true;
    docker-compose-mode.enable = true;
    web-mode = {
      enable = true;
      mode = [
        ''"\\.html\\'"''
      ];
      config = ''
        (setq web-mode-enable-auto-closing t
              web-mode-enable-auto-quoting t
              web-mode-attr-indent-offset 4
              web-mode-code-indent-offset 2
              web-mode-markup-indent-offset 2)
      '';
    };
    js2-mode = {
      enable = true;
      mode = [
        ''"\\.js\\'"''
        ''"\\.json\\'"''
      ];
    };
    scss-mode = {
      enable = true;
      mode = [
        ''"\\.scss\\'"''
      ];
    };
    sass-mode = {
      enable = true;
      mode = [
        ''"\\.sass\\'"''
      ];
    };
    markdown-mode = {
      enable = true;
      mode = [
        ''"\\.markdown\\'"''
        ''"\\.md\\'"''
      ];
    };
    yaml-mode = {
      enable = true;
      mode = [
        ''"\\.yaml\\'"''
        ''"\\.yml\\'"''
      ];
    };
  };
}
