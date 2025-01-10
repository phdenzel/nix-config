{...}: let
  epkgsFn = epkgs:
    with epkgs; [
      s # elisp string methods
      f # file system module
      dash # modern elisp lists (requirement of many other packages)
      hydra # emacs key bindings system
      ivy-hydra # Ivy key bindings
      counsel # misc ivy-enhancements
      swiper # i-search with ivy
      avy # M-s: char-based jumping
      ace-window # window manipulation
      multiple-cursors # multiple cursors
      sudo-edit # sudo edit
      comment-dwim-2 # do what I mean when commenting
      hungry-delete # C-<backspace>: hungry delete backward
      expand-region # C-M-SPC: expand-region
      drag-stuff # C-M-<up>/C-M-<down>: drag regions up and down
      company # completion framework
      # company-jedi
      python-mode # python support
      cython-mode # cython support
      rust-mode # rust support
      nix-mode # nix support
      just-mode # just support
      dockerfile-mode # Dockerfile support
      docker-compose-mode # docker compose support
      web-mode # html support
      js2-mode # javascript support
      scss-mode # scss support
      sass-mode # sass support
      markdown-mode # markdown support
      yaml-mode # yaml support
      lsp-mode # Language Server Protocol support
      pyvenv # select virtual environments
      ein # jupyer notebooks
      # editorconfig # needed for copilot
      # copilot # GitHub copilot
      # tabby # TabbyML self-hosted
      flycheck # syntax checker
      highlight-parentheses # highlight parentheses
      rainbow-delimiters # color parentheses
      rainbow-mode # colorize color strings
      yasnippet # template system
      yasnippet-snippets # collection of snippets
      magit # git magick
      forge # git forges for magit
      projectile # project management
      counsel-projectile # counsel support for projectile
      treemacs # tree layout file explorer
      treemacs-magit # magit plugin for treemacs
      treemacs-projectile # projectile plugin for treemacs
      treemacs-all-the-icons # icon plugin for treemacs
      dashboard # dashboard for startup
      htmlize # buffer style to html
      org # org-mode
      org-bullets # bullet style for org-mode headings
      org-ref # citations and references for org-mode
      ivy-bibtex # bibtex bibliography browsing
      pdf-tools # pdf functionality
      org-roam # knowledge management system
      org-roam-ui # UI for org-roam
      org-roam-bibtex # connect org-roam and ivy-bibtex
      org-re-reveal # org export to reveal.js
      toc-org # auto-refreshing TOC
      ox-gfm # org export to GitHub Flavored Markdown
      ox-rst # org export to reStructuredText
      auctex # latex support
      exec-path-from-shell # proper PATH from shell
      vterm # terminal emulator
      mu4e # emails
      org-mime # html in emails
      pass # zx2c4 pass
      password-store # for pass
      password-store-otp # for pass
      auth-source-xoauth2 # XOAuth2 authentication
      all-the-icons # icon set
      all-the-icons-ivy # icon set for ivy
      all-the-icons-dired # icons set for dired
    ];
in {
  programs.emacs.extraPackages = epkgsFn;
}
