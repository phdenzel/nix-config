{...}: {
  programs.emacs.init.usePackage = {
    org = {
      enable = true;
      custom = {
        org-src-fontify-natively = true;
        org-src-tab-acts-natively = true;
        org-export-backends = "(quote (ascii html latex md))";
      };
      config = ''
        (org-babel-do-load-languages
          'org-babel-load-languages
          '((emacs-lisp . t)
            (shell      . t)
            (makefile   . t)
            (python     . t)
            (C          . t)
            (js         . t)
            (dot        . t)
            (gnuplot    . t)))
      '';
    };
    org-bullets = {
      enable = true;
      after = ["org"];
      commands = ["org-bullets-mode"];
      hook = ["(org-mode . org-bullets-mode)"];
    };
    org-id = {
      enable = true;
      custom = {
        org-id-link-to-org-use-id = "'create-if-interactive-and-no-custom-id";
      };
    };
    org-ref = {
      enable = true;
      init = ''
        (require 'bibtex)
        (require 'org-ref-ivy)
        (require 'org-ref-bibtex)
        (require 'org-ref-arxiv)
        (require 'org-ref-isbn)
        (require 'org-ref-scopus)
        (require 'doi-utils)
      '';
    };
    org-ref-ivy = {
      enable = true;
      init = ''
        (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
             org-ref-insert-cite-function 'org-ref-cite-insert-ivy
             org-ref-insert-label-function 'org-ref-insert-label-link
             org-ref-insert-ref-function 'org-ref-insert-ref-link
             org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
        (defhydra+ org-ref-insert-link-hydra (:color red :hint nil)
          "Add arxiv entry from DOI to `org-ref-insert-link-hydra'."
          ("ba" phd/bibtex-entry-from-arxiv-doi "Add bibtex entry from arXiv DOI" :column "Bibtex"))'';
    };
    org-roam = {
      enable = true;
      custom = {
        org-roam-directory = "(file-truename \"~/zettelkasten\")";
      };
      config = ''
        (unless (file-exists-p org-roam-directory)
          (make-directory org-roam-directory t))
        (org-roam-db-autosync-mode)'';
    };
    org-roam-bibtex = {
      enable = true;
      after = ["org-roam"];
      custom = {
        orb-roam-ref-format = "'org-ref-v3";
        orb-preformat-keywords = '''("citekey" "author-or-editor-abbrev" "year" "author" "doi" "file")'';
      };
      config = "\t(require 'org-ref)\n\t(add-to-list 'org-roam-capture-templates\n\t\t'(\"r\" \"bibliographic reference\" plain\n\t\t  \"%?\"\n\t\t  :target (file+head \"ref/\${citekey}.org\" \"#+title: \${author-or-editor-abbrev} (\${year} )\\n*Authors*: \${author}\\n\\n*DOI*: [[\${doi} ] ]\\n*PDF*: [[\${file} ] ]\\n\\n* \${title}\\n\")\n\t\t  :unnarrowed t))";
    };
    org-roam-ui = {
      enable = true;
      after = ["org-roam"];
    };
    org-re-reveal = {
      enable = true;
      after = ["org"];
      custom = {
        org-re-reveal-root = "(expand-file-name \"~/local/reveal.js\")";
        org-re-reveal-plugins = "'(highlight markdown math notes search zoom)";
      };
    };
    htmlize.enable = true;
    toc-org = {
      enable = true;
      hook = [
        "(org-mode . toc-org-mode)"
        "(markdown-mode . toc-org-mode)"
      ];
    };
    org-mime = {
      enable = true;
      config = ''
        (setq org-mime-export-options '(:section-numbers nil
                                        :with-author nil
                                        :with-toc nil))
      '';
    };
  };
}
