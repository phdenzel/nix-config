{...}: {
  programs.emacs.init.usePackage = {
    jinx = {
      enable = true;
      hook = [
        "(text-mode . jinx-mode)"
        "(LaTeX-mode . jinx-mode)"
        "(latex-mode . jinx-mode)"
        "(bibtex-mode . jinx-mode)"
        "(org-mode . jinx-mode)"
      ];
    };
    auctex = {
      enable = true;
      defer = true;
      hook = [
        "(LaTeX-mode . prettify-symbols-mode)"
        "(LaTeX-mode . reftex-mode)"
        "(LaTeX-mode . outline-minor-mode)"
      ];  
    };
    tex = {
      enable = true;
      defer = true;
      custom = {
        TeX-auto-save = true;
        TeX-parse-self = true;
        TeX-master = false;
        TeX-view-program-selection = "'((output-pdf \"PDF Tools\"))";
        # TeX-view-program-selection = "'((output-pdf \"Zathura\"))";
      };
      config = ''
        ;; Compilation commands
        (setq TeX-command-default "latexmk")
        (add-to-list 'TeX-command-list
          '("latexmk clean" "latexmk -quiet -c %s" TeX-run-TeX nil t
            :help "Run latexmk clean"))
        (add-to-list 'TeX-command-list
          '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
            :help "Run latexmk on file"))
        ;; Reload after compile
        (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
      '';
    };
    reftex = {
      enable = true;
      defer = true;
      custom = {
        reftex-toc-split-windows-horizontally = true;
        reftex-toc-split-windows-fraction = 0.2;
        reftex-plug-into-AUCTeX = true;
      };
    };
    pdf-tools = {
      enable = true;
      defer = true;
      mode = [
        ''("\\.pdf\\'" . pdf-view-mode)''
      ];
      init = ''(pdf-loader-install)'';
      config = ''
        (setq pdf-view-use-scaling t)
      '';
      bindLocal = {
        pdf-view-mode-map = {
          "C-s" = "isearch-forward";
          "C-M-s" = "pdf-occur";
        };
      };
    };
  };
}
