{...}: {
  programs.emacs.init.usePackage = {
    # LSP features
    lsp-mode = {
      enable = true;
      defer = true;
      commands = ["lsp" "lsp-deferred"];
      hook = [
        "(python-mode . lsp)"
        "(rust-mode . lsp)"
        "(nix-mode . lsp-deferred)"
        "(c-mode   . lsp-deferred)"
        "(c++-mode . lsp-deferred)"
        "(LaTeX-mode . lsp)"
        "(latex-mode . lsp)"
      ];
      init = ''
        (setq lsp-keymap-prefix "C-c l")
        ;; (setq lsp-use-plists t)
      '';
      custom = {
        lsp-auto-guess-root = true; # avoid projectile bookmark prompt
      };
    };
    lsp-completion = {
      # https://github.com/MasseR/nix-conf-emacs/commit/f4287c2b34128b0dde61f58ada4e474e1ed096dc
      enable = true;
      config = ''
        (lsp-inline-completion-mode nil)
      '';
    };
    lsp-rust = {
      enable = true;
      after = ["lsp-mode"];
    };
    lsp-ruff = {
      # used as plugin in lsp-pylsp
      enable = false;
      after = ["lsp-mode"];
    };
    lsp-pylsp = {
      enable = true;
      after = ["lsp-mode"];
      custom = {
        lsp-pylsp-configuration-sources = "[\"ruff\"]";
        lsp-pylsp-plugins-jedi-completion-enabled = true;
        lsp-pylsp-plugins-isort-enabled = true;
        lsp-pylsp-plugins-ruff-enabled = true;
        lsp-pylsp-plugins-mypy-enabled = true;
        lsp-pylsp-rename-backend = "'rope";
        lsp-pylsp-plugins-mccabe-enabled = false;
        lsp-pylsp-plugins-pylint-enabled = false;
        lsp-pylsp-plugins-pycodestyle-enabled = false;
        lsp-pylsp-plugins-pydocstyle-enabled = false;
        lsp-pylsp-plugins-pyflakes-enabled = false;
        lsp-pylsp-plugins-rope-autoimport-enabled = false;
        lsp-pylsp-plugins-rope-completion-enabled = false;
        lsp-pylsp-plugins-autopep8-enabled = false;
        lsp-pylsp-plugins-yapf-enabled = false;
        lsp-pylsp-plugins-black-enabled = false;
        lsp-pylsp-plugins-flake8-enabled = false;
      };
    };
    lsp-nix = {
      enable = true;
      after = ["lsp-mode"];
      custom = {lsp-nix-nil-formatter = "[\"alejandra\"]";};
    };
    lsp-tex = {
      enable = true;
      after = ["lsp-mode"];
    };
    lsp-ivy = {
      enable = false;
      after = ["lsp-mode"];
      commands = ["lsp-ivy-workspace-symbol"];
    };
    lsp-ui = {
      enable = true;
      after = ["lsp-mode"];
      commands = ["lsp-ui-mode"];
      config = ''
        (setq lsp-ui-sideline-enable nil
              lsp-ui-sideline-show-symbol nil
              lsp-ui-sideline-show-hover nil
              lsp-ui-sideline-show-code-actions nil
              lsp-ui-sideline-update-mode 'point
              lsp-ui-doc-enable t
              lsp-ui-doc-delay 2
              lsp-ui-doc-show-with-cursor t)'';
    };
    lsp-treemacs = {
      enable = true;
      after = ["lsp-mode" "treemacs"];
      commands = ["lsp-treemacs-errors-list" "lsp-treemacs-symbols"];
      config = ''
        (lsp-treemacs-sync-mode t)
        (setq lsp-treemacs-deps-position-params
              '((side . right)
                (slot . 1)
                (window-width . 35)))
        (setq lsp-treemacs-symbols-position-params
              '((side . right)
                (slot . 2)
                (window-width . 35)))'';
    };
    dap-mode = {
      enable = true;
      after = ["lsp-mode"];
      commands = ["dap-mode" "dap-ui-mode"];
    };
    dap-python = {
      enable = true;
      after = ["lsp-mode"];
      config = ''
        (setq dap-python-debugger 'debugpy)
      '';
    };
    dap-gdb-lldb = {
      enable = true;
      after = ["lsp-mode"];
      config = ''
        (dap-register-debug-template "Rust::GDB Run Configuration"
                             (list :type "gdb"
                                   :request "launch"
                                   :name "GDB::Run"
                           :gdbpath "rust-gdb"
                                   :target nil
                                   :cwd nil))'';
    };
    pyvenv = {
      enable = true;
      config = ''
        (pyvenv-mode t)
        (pyvenv-tracking-mode t)'';
    };
    company-jedi = {
      # find workaround for jedi-setup
      enable = false;
      config = ''
        (add-hook 'python-mode-hook 'jedi:setup)
        (setq jedi:complete-on-dot t)
        (setq jedi:use-shortcuts t)
        (defun phd/enable-company-jedi ()
          (add-to-list 'company-backends 'company-jedi))
        (add-hook 'python-mode-hook 'phd/enable-company-jedi)'';
    };
    ein = {
      enable = true;
      custom = {
        "ein:output-area-inlined-images" = true;
      };
    };

    # Languages
    python-mode.enable = false; # builtin python-mode is better
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
    cuda-mode = {
      enable = true;
      mode = [''"\\.cu\\'"''];
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
              '("--ghc-options=+RTS -M500m -RTS -ferror-spans -fshow-loaded-modules"))'';
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
      custom = {
        web-mode-enable-auto-closing = true;
        web-mode-enable-auto-quoting = true;
        web-mode-attr-indent-offset = 4;
        web-mode-code-indent-offset = 2;
        web-mode-markup-indent-offset = 2;
      };
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

    # Syntax
    flycheck = {
      enable = true;
      commands = ["global-flycheck-mode"];
      custom = {
        flycheck-check-syntax-automatically = "'(mode-enabled save)";
        flycheck-flake8-maximum-line-length = 99;
        flycheck-disabled-checkers = "'(python-pylint)";
      };
      config = "(global-flycheck-mode t)";
    };
  };
}
