{pkgs, ...}: let
  packages = epkgs:
    with epkgs; [
      # Elisp packages
      s # elisp string methods
      f # file system module
      dash # modern elisp lists (requirement of many other packages)

      # Navigation packages
      hydra # emacs key bindings system
      ivy-hydra # Ivy key bindings
      counsel # misc ivy-enhancements
      swiper # i-search with ivy
      avy # M-s: char-based jumping
      ace-window # window manipulation

      # Editing packages
      multiple-cursors # multiple cursors
      sudo-edit # sudo edit
      comment-dwim-2 # do what I mean when commenting
      hungry-delete # C-<backspace>: hungry delete backward
      expand-region # C-M-SPC: expand-region
      drag-stuff # C-M-<up>/C-M-<down>: drag regions up and down
      company # completion framework
      #company-jedi  # completion for python
      yasnippet # template system
      yasnippet-snippets # collection of snippets

      # Project managment packages
      magit # git magick
      forge # git forges for magit
      projectile # project management
      counsel-projectile # counsel support for projectile
      treemacs # tree layout file explorer
      treemacs-magit # magit plugin for treemacs
      treemacs-projectile # projectile plugin for treemacs

      # Cytpography
      pass # zx2c4 pass

      # Dev packages
      lsp-mode # Language Server Protocol support
      lsp-ui # Language Server Protocol UI support
      #lsp-ivy # Ivy symbol support; currently borked: https://github.com/emacs-lsp/lsp-ivy/issues/27
      lsp-treemacs # Language Server Protocol treemacs
      dap-mode # Debugger
      pyvenv # select virtual environments
      ein # jupyer notebooks
      #python-mode # custom mode different from built-in python-mode
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
      flycheck # syntax checker

      # Shell packages
      vterm # terminal emulator
      #exec-path-from-shell # proper PATH from shell

      # Org-mode packages
      org # org-mode
      org-bullets # bullet style for org-mode headings
      org-ref # citations and references for org-mode
      org-roam # knowledge management system
      org-roam-bibtex # connect org-roam and ivy-bibtex
      org-roam-ui # UI for org-roam
      org-re-reveal # org export to reveal.js
      biblio # paper lookup
      htmlize # buffer style to html
      toc-org # auto-refreshing TOC
      org-mime # html in emails

      # Typesetting packages
      jinx # spell checking using enchant API
      auctex # latex support
      pdf-tools # pdf functionality
      ivy-bibtex # bibtex bibliography browsing

      # AI
      editorconfig # needed for copilot
      jsonrpc # needed for copilot
      copilot # GitHub copilot
      ellama # Ollama self-hosted LLMs
      aidermacs # aider interface

      # Mail
      himalaya # himalaya frontend
      mu4e # emails

      # Style packages
      #highlight-parentheses # rather use rainbow-delimiters
      rainbow-delimiters # color parentheses
      rainbow-mode # colorize color strings
      all-the-icons # icon set
      all-the-icons-ibuffer # icon set for ibuffer
      all-the-icons-ivy # icon set for ivy
      all-the-icons-dired # icons set for dired
      all-the-icons-nerd-fonts # nerd-font bridge
      treemacs-all-the-icons # icon plugin for treemacs
      dashboard # dashboard for startup
    ];
in {
  programs.emacs.extraPackages = packages;
  # epkgs:
  #   (packages (epkgs.overrideScope (ff: pp: {
  #     lsp-mode = (
  #       pp.lsp-mode.overrideAttrs (f: p: {
  #         buildPhase = ''export LSP_USE_PLISTS=true''+ p.buildPhase;
  #       }));
  #     lsp-ui = (
  #       pp.lsp-mode.overrideAttrs (f: p: {
  #         buildPhase = ''export LSP_USE_PLISTS=true''+ p.buildPhase;
  #       }));
  #     lsp-treemacs = (
  #       pp.lsp-mode.overrideAttrs (f: p: {
  #         buildPhase = ''export LSP_USE_PLISTS=true''+ p.buildPhase;
  #       }));
  #     dap-mode = (
  #       pp.lsp-mode.overrideAttrs (f: p: {
  #         buildPhase = ''export LSP_USE_PLISTS=true''+ p.buildPhase;
  #       }));
  # })));
  home.packages = with pkgs; [
    alejandra
    copilot-node-server
    emacs-all-the-icons-fonts
    enchant
    hunspellDicts.de-de
    hunspellDicts.en-us
    hunspellDicts.en-gb-ize
    jansson
    nil
    nuspell
    texlab
  ];
}
