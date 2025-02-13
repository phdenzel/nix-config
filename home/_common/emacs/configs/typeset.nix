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
      after = ["auctex"];
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
        (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
      '';
    };
    latex = {
      enable = true;
      defer = true;
      after = ["tex"];
    };
    reftex = {
      enable = true;
      defer = true;
      after = ["auctex" "tex"];
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
      hook = ["(pdf-view-mode . pdf-view-midnight-minor-mode)"];
      init = ''(pdf-loader-install)'';
      custom = {
        pdf-view-use-scaling = true;
      };
      bindLocal = {
        pdf-view-mode-map = {
          "C-s" = "isearch-forward";
          "C-M-s" = "pdf-occur";
        };
      };
    };
    bibtex = {
      enable = true;
      custom = {
        # bibtex key format definition
        bibtex-autokey-year-length = 2;
        bibtex-autokey-name-case-convert-function = "'capitalize";
        bibtex-autokey-name-year-separator = "\"\"";
        bibtex-autokey-year-title-separator = "\"\"";
        bibtex-autokey-titleword-separator = "\"\"";
        bibtex-autokey-titlewords = 0;
        bibtex-autokey-titlewords-stretch = 0;
        bibtex-autokey-titleword-length = 0;
      };
    };
    bibtex-completion = {
      enable = true;
      custom = {
        bibtex-completion-bibliography = '''("~/zettelkasten/bib/master.bib")'';
        bibtex-completion-library-path = '''("~/zettelkasten/pdf")'';
        bibtex-completion-pdf-symbol = "\"⌘\"";
        bibtex-completion-pdf-field = "\"file\"";
        bibtex-completion-notes-path = ''"~/zettelkasten/ref/"'';
        bibtex-completion-notes-symbol = "\"✎\"";
        bibtex-completion-notes-template-multiple-files = "\"* $\{author-or-editor-abbrev} ($\{year}) - $\{title}\\n\\nReference: [[cite:&$\{=key=}]]\\n\"";
        bibtex-completion-format-citation-functions = ''
          '((org-mode . bibtex-completion-format-citation-org-cite)
            (latex-mode . bibtex-completion-format-citation-cite)
            (LaTeX-mode . bibtex-completion-format-citation-cite)
            (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (python-mode   . bibtex-completion-format-citation-sphinxcontrib-bibtex)
            (rst-mode      . bibtex-completion-format-citation-sphinxcontrib-bibtex)
            (default       . bibtex-completion-format-citation-default))'';
        bibtex-completion-display-formats = "'((t . \"\${=has-pdf=:1}\${=has-note=:1} \${=key=:16} \${=type=:9} \${author:16} \${year:4} \${title:44}\"))";
      };
    };
    ivy-bibtex = {
      enable = true;
      after = ["ivy" "bibtex-completion"];
      init = ''
        (setq ivy-re-builders-list
                '((ivy-bibtex . ivy--regex-ignore-order)
                  (t . ivy--regex-plus)))'';
      custom = {
        ivy-bibtex-default-action = "'ivy-bibtex-insert-citation";
        ivy-bibtex-default-multi-action = "'ivy-bibtex-insert-citation";
      };
    };
  };
}
