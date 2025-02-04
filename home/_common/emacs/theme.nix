{
  config,
  lib,
  ...
}:
with lib; let
  cfg = config.emacs.custom-theme;
in {
  options.emacs.custom-theme = {
    enable = mkEnableOption "Enable the custom emacs theme.";
    fileHeader = mkOption {
      type = types.str;
      default = ''
        ;;; ${cfg.name}-theme.el --- phdenzel's ${cfg.name} theme for Emacs -*- lexical-binding: t; -*-

        ;; Copyright 2019-present, All rights reserved
        ;;
        ;; Code licensed under the MIT license

        ;; Author: phdenzel
        ;; Maintainer: phdenzel
        ;; Version: 1.0.0
        ;; Package-Requires: ((emacs "25.1"))
        ;; URL: https://github.com/phdenzel/nix-config/tree/main/home/_common/emacs/theme.nix

        ;;; Commentary:
        ;; Should also work in Terminal.
        ;;

        ;;; Code:
      '';
      example = "";
      description = "An emacs theme file header with comments.";
    };
    name = mkOption {
      type = types.str;
      example = "my-awesome-theme";
      default = "phd-ark";
    };
    defCustoms = mkOption {
      type = types.attrs;
      example = literalExpression ''
        {
          height-title-1 = {
            default = 1;
            description = "Header 1 font size";
            type = "number";
            group = "my-theme";
          };
        };
      '';
      default = {
        "${cfg.name}-enlarge-headings" = {
          default = "t";
          description = "Use different font sizes for some headings and titles.";
          type = "boolean";
        };
        "${cfg.name}-height-title-1" = {
          default = "1.0";
          description = "Header 1 font size.";
          type = "number";
        };
        "${cfg.name}-height-title-2" = {
          default = "1.0";
          description = "Header 2 font size.";
          type = "number";
        };
        "${cfg.name}-height-title-3" = {
          default = "1.0";
          description = "Header 3 font size.";
          type = "number";
        };
        "${cfg.name}-height-doc-title" = {
          default = "1.0";
          description = "Documentation title font size.";
          type = "number";
        };
        "${cfg.name}-highlight-matches" = {
          default = "nil";
          description = "Use background color to make highlighted matches more visible.";
          type = "boolean";
        };
        "${cfg.name}-italic-comments" = {
          default = "nil";
          description = "Use :slant italic for comments.";
          type = "boolean";
        };
        "${cfg.name}-italic-blockquotes" = {
          default = "t";
          description = "Use :slant italic for variables.";
          type = "boolean";
        };
        "${cfg.name}-italic-variables" = {
          default = "nil";
          description = "Use :slant italic for variables.";
          type = "boolean";
        };
        "${cfg.name}-polarity" = {
          default = "'dark";
          description = ''
            The flavor to use for the phd-ark theme.
            Must be one of `dark` or `light`.'';
          type = ''
            (choice (const :tag "Dark" dark)
                    (const :tag "Light" light))'';
        };
      };
    };
    defVars = mkOption {
      type = types.attrs;
      example = literalExpression ''
        {
          use-24-bit-colors = {
            default = "nil";
          };
        };
      '';
      default = {
        "${cfg.name}-use-24-bit-colors-on-256-colors-terms" = {
          default = "nil";
          description = ''
            Use true colors even on terminals announcing less capabilities.

            Beware the use of this variable.  Using it may lead to unwanted
            behavior, the most common one being an ugly blue background on
            terminals, which don't understand 24 bit colors.  To avoid this
            blue background, when using this variable, one can try to add the
            following lines in their config file after having loaded the
            ${cfg.name} theme:

            (unless (display-graphic-p)
              (set-face-background \\='default \"black\" nil))
          '';
        };
      };
    };
    deFuns = mkOption {
      type = types.attrs;
      example = literalExpression ''
        {
          theme/reload = {
            args = "color";
            description = "";
            interactive = true;
            body = "(disable-theme '${cfg.name})\n(load-theme '${cfg.name} t)";
          };
        };
      '';
      default = {
        "${cfg.name}-rnd" = {
          args = "n";
          description = "Round N to next integer.";
          body = "(round (+ .5 n))";
        };
        "${cfg.name}-rgb-to-hex" = {
          args = "r g b";
          description = "Convert R G B channels into hex format.";
          body = "(format \"#%02x%02x%02x\" r g b)";
        };
        "${cfg.name}-hex-to-rgb" = {
          args = "color";
          description = "Convert hex COLOR into RGB format.";
          body = ''
            (mapcar (lambda (i)
                      (string-to-number (substring color i (+ i 2)) 16))
                    '(1 3 5))'';
        };
        "${cfg.name}-lighten" = {
          args = "color value";
          description = "Lighten COLOR by VALUE%.";
          body = ''
            (let* ((factor (/ value 100.0)))
              (apply ${cfg.name}-rgb-to-hex
                     (mapcar
                      (lambda (v) (funcall ${cfg.name}-rnd (min 255 (+ (* (- 255 v) factor) v))))
                      (funcall ${cfg.name}-hex-to-rgb color))))
          '';
        };
        "${cfg.name}-darken" = {
          args = "color value";
          description = "Darken COLOR by VALUE%.";
          body = ''
            (let* ((factor (/ value 100.0)))
              (apply ${cfg.name}-rgb-to-hex
                     (mapcar (lambda (v) (floor (* (- 1 factor) v)))
                             (funcall ${cfg.name}-hex-to-rgb color))))
          '';
        };
        "${cfg.name}-quantize-color" = {
          args = "color";
          description = "Quantize COLOR to a 256 color palette.";
          body = ''
            (let ((i 1)
                  (str "#"))
              (while (<= i 5)
                (setq str (concat str (format "%02x" (* (round (/ (string-to-number (substring color i (+ i 2)) 16) 17)) 17))))
                (setq i (+ i 2)))
              str)
          '';
        };
        "${cfg.name}/reload" = {
          description = "Reload the ${cfg.name} theme, useful after `${cfg.name}/set-color`.";
          interactive = true;
          body = ''
            (disable-theme '${cfg.name})
            (load-theme '${cfg.name} t)'';
        };
        "${cfg.name}/load-polarity" = {
          args = "polarity";
          description = "Set the theme polarity (e.g. dark or light) to POLARITY.";
          interactive = ''
            (list (intern (completing-read "${cfg.name} polarity: "
              '(${concatStringsSep " " (attrNames cfg.themePalettes)}) nil t)))'';
          body = ''
            (setq ${cfg.name}-polarity polarity)
            (${cfg.name}/reload)
            (message "${cfg.name} polarity changed to %s" polarity)
          '';
        };
        "${cfg.name}/get-color" = {
          args = "color &optional polarity";
          description = "Get the COLOR of POLARITY or the current flavor.";
          interactive = "\"SThe color to get: \"";
          body = ''
            (let ((result (alist-get color
                   (symbol-value (intern-soft
                     (concat "${cfg.name}-"
                       (symbol-name (or polarity ${cfg.name}-polarity))
                                    "-colors"))))))
              (if (called-interactively-p 'interactive)
                (message result)
              result))'';
        };
        "${cfg.name}/set-color" = {
          args = "color value &optional polarity";
          description = "Set the COLOR of POLARITY or the current flavor to VALUE.";
          interactive = "\"SChange color: \nsSet %s to: \"";
          body = ''
            (setcdr
             (assoc color (symbol-value
                           (intern-soft
                            (concat "${cfg.name}-"
                                     (symbol-name (or polarity ${cfg.name}-polarity))
                          "-colors")))) value)'';
        };
      };
    };
    themePalettes = mkOption {
      type = types.attrs;
      default = with config.colorScheme; {
        dark = palette;
        light =
          palette
          // {
            base = palette.subtext0;
            mantle = palette.subtext1;
            crust = palette.text;
            subtext0 = palette.base;
            subtext1 = palette.mantle;
            text = palette.crust;
            overlay0 = palette.overlay2;
            overlay2 = palette.overlay0;
            surface0 = palette.surface2;
            surface2 = palette.surface0;
          };
      };
    };
    themeFaces = mkOption {
      type = types.attrs;
      example = literalExpression ''
        {
          default = {
            fg = "text";
            bg = "base";
          }
        }
      '';
      default = {
        # Basic faces
        default = {
          bg = "base";
          fg = "text";
        };
        cursor = {bg = "blue";};
        escape-glyph = {fg = "orange";};
        minibuffer-prompt = {fg = "teal";};
        highlight = {bg = "overlay1";};
        hl-line = {from = "highlight";};
        region = {bg = "pink";};
        ffap = {
          from = "default";
          fg = "overlay0";
        };
        fringe = {
          from = "default";
          fg = "overlay0";
        };
        shadow = {fg = "overlay2";};
        secondary-selection = {
          bg = "blue";
          fg = "text";
        };
        lazy-highlight = {from = "secondary-selection";};
        match = {from = "secondary-selection";};
        next-error = {from = "highlight";};
        query-replace = {from = "isearch";};
        trailing-whitespace = {bg = "base";};
        linum = {
          bg = "base";
          fg = "overlay1";
        };
        tooltip = {
          from = "variable-pitch";
          bg = "overlay1";
          fg = "text";
        };
        link = {
          fg = "purple";
          ul = {
            color = "purple";
            style = "line";
          };
        };
        link-visited = {
          fg = "violet";
          ul = {
            color = "violet";
            style = "line";
          };
        };
        button = {from = "link";};
        error = {fg = "ruby";};
        warning = {fg = "orange";};
        success = {fg = "emerald";};
        show-paren-match = {
          bg = "blue";
          fg = "crust";
        };
        show-paren-mismatch = {
          bg = "sand";
          fg = "crust";
        };

        # Customize group
        custom-state = {fg = "green";};
        widget-field = {bg = "overlay1";};

        # Font locks
        font-lock-builtin-face = {fg = "cyan";};
        font-lock-preprocessor-face = {from = "font-lock-builtin-face";};
        font-lock-constant-face = {fg = "purple";};
        font-lock-function-name-face = {fg = "blue";};
        font-lock-keyword-face = {fg = "pink";};
        font-lock-type-face = {fg = "yellow";};
        font-lock-variable-name-face = {fg = "sand";};
        font-lock-string-face = {fg = "green";};
        font-lock-doc-face = {from = "font-lock-string-face";};
        font-lock-doc-string-face = {from = "font-lock-string-face";};
        font-lock-comment-face = {fg = "overlay2";};
        font-lock-comment-delimiter-face = {from = "font-lock-comment-face";};
        font-lock-negation-char-face = {nil = true;};
        font-lock-preprocessor-char-face = {nil = true;};
        font-lock-warning-face = {from = "warning";};
        font-lock-regexp-grouping-backslash = {from = "(bold)";};
        font-lock-regexp-grouping-construct = {from = "(bold)";};

        # Ansi colors
        ansi-color-black = {fg = "base";};
        ansi-color-red = {fg = "red";};
        ansi-color-green = {fg = "green";};
        ansi-color-yellow = {fg = "yellow";};
        ansi-color-blue = {fg = "blue";};
        ansi-color-magenta = {fg = "pink";};
        ansi-color-cyan = {fg = "teal";};
        ansi-color-gray = {fg = "subtext0";};
        ansi-color-bright-black = {fg = "surface1";};
        ansi-color-bright-red = {fg = "ruby";};
        ansi-color-bright-green = {fg = "emerald";};
        ansi-color-bright-yellow = {fg = "sand";};
        ansi-color-bright-blue = {fg = "indigo";};
        ansi-color-bright-magenta = {fg = "magenta";};
        ansi-color-bright-cyan = {fg = "cyan";};
        ansi-color-bright-gray = {fg = "white";};

        # Org mode
        org-block = {bg = "mantle";};
        org-block-background = {bg = "mantle";};
        org-code = {from = "org-block";};

        # Org ref
        org-ref-ref-face = {fg = "purple";};
        org-ref-cite-face = {fg = "cyan";};
        org-ref-label-face = {fg = "viridis";};
        org-ref-bad-cite-key-face = {fg = "pink";};

        # Latex
        font-latex-sedate-face = {fg = "pink";};
        font-latex-sectioning-1-face = {from = "outline-1";};
        font-latex-sectioning-2-face = {from = "outline-1";};
        font-latex-sectioning-3-face = {from = "outline-2";};
        font-latex-sectioning-4-face = {from = "outline-4";};
        font-latex-sectioning-5-face = {from = "outline-5";};
        font-latex-italic-face = {fg = "green";};
        font-latex-math-face = {fg = "sand";};
        font-latex-warning-face = {fg = "blue";};

        # JS2
        js2-external-variable = {fg = "yellow";};
        js2-function-param = {fg = "violet";};
        js2-instance-member = {fg = "teal";};
        js2-jsdoc-html-tag-delimiter = {fg = "green";};
        js2-jsdoc-html-tag-name = {fg = "yellow";};
        js2-jsdoc-tag = {fg = "purple";};
        js2-jsdoc-type = {fg = "teal";};
        js2-jsdoc-value = {fg = "violet";};
        js2-error = {
          ul = {
            style = "wave";
            color = "ruby";
          };
        };
        js2-warning = {
          ul = {
            style = "wave";
            color = "orange";
          };
        };

        # Tab-line
        tab-line = {
          bg = "surface0";
          fg = "text";
          height = "120";
        };
        tab-line-tab = {
          from = "tab-line";
          bg = "base";
        };
        tab-line-tab-current = {
          from = "tab-line-tab";
          fg = "teal";
        };
        tab-line-tab-inactive = {
          from = "tab-line";
          fg = "overlay0";
        };
        tab-line-highlight = {
          from = "tab-line";
          fg = "text";
          bg = "base";
        };
        tab-line-tab-modified = {
          from = "tab-line-current";
          fg = "blue";
        };

        # Mode-line
        mode-line = {
          bg = "surface0";
          fg = "subtext1";
        };
        mode-line-highlight = {
          bg = "surface0";
          fg = "subtext0";
        };
        mode-line-inactive = {
          bg = "mantle";
          fg = "text";
        };
        mode-line-emphasis = {nil = true;};
        mode-line-buffer-id = {nil = true;};
        header-line = {from = "mode-line";};

        # phd-modeline
        phd-modeline-buffer-name-face = {fg = "teal";};
        phd-modeline-buffer-modified-face = {fg = "blue";};
        phd-modeline-buffer-read-only-face = {fg = "pink";};
        phd-modeline-buffer-line-face = {fg = "subtext1";};
        phd-modeline-buffer-column-face = {fg = "subtext1";};
        phd-modeline-buffer-percentage-face = {fg = "subtext1";};
        phd-modeline-mode-face = {fg = "blue";};
        phd-modeline-flycheck-success-face = {fg = "grass";};
        phd-modeline-flycheck-warning-face = {fg = "orange";};
        phd-modeline-flycheck-error-face = {fg = "ruby";};
        phd-modeline-vc-icon-face = {fg = "pink";};
        phd-modeline-vc-branch-face = {fg = "sand";};
        phd-modeline-vc-status-face = {fg = "purple";};
        phd-modeline-mail-icon-face = {fg = "grass";};
        phd-modeline-mail-status-face = {fg = "text";};
        phd-modeline-inactive-face = {from = "mode-line-inactive";};
        phd-modeline-bar-face = {
          fg = "teal";
          bg = "surface0";
        };

        # Treemacs
        treemacs-directory-face = {fg = "blue";};
        treemacs-file-face = {from = "default";};
        treemacs-root-face = {
          fg = "teal";
          bold = true;
          height = "1.1";
        };
        treemacs-git-added-face = {fg = "grass";};
        treemacs-git-modified-face = {fg = "pink";};
        treemacs-git-renamed-face = {fg = "teal";};
        treemacs-git-untracked-face = {fg = "cyan";};
        treemacs-git-conflict-face = {from = "error";};
        treemacs-all-the-icons-file-face = {fg = "blue";};
        treemacs-all-the-icons-root-face = {fg = "teal";};

        # Perspeen
        perspeen-tab--header-line-active = {
          bg = "blue";
          from = "mode-line";
        };
        perspeen-selected-face = {
          bg = "blue";
          from = "mode-line";
        };

        # (i)Search
        isearch = {from = "region";};
        isearch-fail = {from = "highlight";};
        yas-field-highlight-face = {from = "match";};

        # Swiper
        swiper-line-face = {
          bg = "blue";
          fg = "crust";
        };
        swiper-match-face-1 = {
          bg = "crust";
          fg = "overlay0";
        };
        swiper-match-face-2 = {
          bg = "pink";
          fg = "white :bold t";
        };
        swiper-match-face-3 = {
          bg = "sand";
          fg = "crust :bold t";
        };
        swiper-match-face-4 = {
          bg = "teal";
          fg = "white :bold t";
        };

        # Ivy
        ivy-current-match = {
          fg = "crust";
          bg = "blue";
        };
        ivy-minibuffer-match-face-1 = {fg = "crust";};
        ivy-minibuffer-match-face-2 = {fg = "pink";};
        ivy-minibuffer-match-face-3 = {fg = "sand";};
        ivy-minibuffer-match-face-4 = {fg = "teal";};
        all-the-icons-ivy-dir-face = {fg = "blue";};

        # Avy
        avy-lead-face = {
          bg = "blue";
          fg = "base";
        };
        avy-lead-face-0 = {
          bg = "pink";
          fg = "base";
        };
        avy-lead-face-1 = {
          bg = "sand";
          fg = "base";
        };
        avy-lead-face-2 = {
          bg = "teal";
          fg = "text";
        };

        # Ace-window
        aw-leading-char-face = {
          fg = "teal";
          bold = true;
          height = "2.0";
        };
        aw-minibuffer-leading-char-face = {
          fg = "teal";
          bold = true;
          height = "2.0";
        };
        aw-mode-line-face = {
          fg = "viridis";
          bold = true;
        };

        # Company
        company-tooltip = {from = "tooltip";};
        company-tooltip-common = {fg = "teal";};
        company-tooltip-common-selection = {from = "company-tooltip-common";};
        company-tooltip-selection = {bg = "pink";};
        company-tooltip-search = {from = "secondary-selection";};
        company-tooltip-annotation = {fg = "cyan";};
        company-tooltip-mouse = {from = "secondary-selection";};
        company-scrollbar-bg = {bg = "teal";};
        company-scrollbar-fg = {bg = "ocean";};
        company-preview = {from = "company-tooltip-selection";};
        company-preview-common = {from = "company-tooltip-common";};
        company-preview-search = {from = "company-tooltip-search";};

        # which-key
        which-key-key-face = {fg = "green";};
        which-key-group-description-face = {fg = "violet";};
        which-key-command-description-face = {fg = "blue";};
        which-key-local-map-description-face = {fg = "pink";};

        # LSP
        lsp-ui-doc-background = {bg = "mantle";};
        lsp-ui-doc-header = {fg = "teal";};
        lsp-headerline-breadcrumb-symbols-face = {fg = "teal";};
        lsp-headerline-breadcrumb-path-face = {fg = "teal";};
        lsp-headerline-breadcrumb-path-error-face = {
          fg = "surface1";
          style = "nil";
        };
        lsp-headerline-breadcrumb-symbols-error-face = {
          fg = "surface1";
          style = "nil";
        };
        lsp-headerline-breadcrumb-path-warning-face = {from = "lsp-headerline-breadcrumb-path-face";};
        lsp-headerline-breadcrumb-symbols-warning-face = {from = "lsp-headerline-breadcrumb-symbols-face";};
        lsp-headerline-breadcrumb-deprecated-face = {
          fg = "surface0";
          style = "nil";
        };
        lsp-headerline-breadcrumb-path-info-face = {fg = "blue";};
        lsp-headerline-breadcrumb-symbols-info-face = {fg = "blue";};
        lsp-headerline-breadcrumb-project-prefix-face = {
          fg = "blue";
          bold = true;
        };
        lsp-headerline-breadcrumb-unknown-project-prefix-face = {fg = "cyan";};
        lsp-modeline-code-actions-preferred-face = {fg = "yellow";};
        lsp-modeline-code-actions-face = {fg = "cyan";};
        lsp-installation-buffer-face = {fg = "green";};
        lsp-installation-finished-buffer-face = {fg = "orange";};

        # Flycheck/-make/-spell
        flycheck-error = {nil = true;};
        flycheck-warning = {nil = true;};
        flycheck-info = {nil = true;};
        flymake-errline = {nil = true;};
        flyspell-incorrect = {nil = true;};
        flyspell-duplicate = {nil = true;};
        # flycheck-error = {ul = {style = "wave"; color = "red";};};
        # flycheck-warning = {ul = {style = "wave"; color = "yellow";};};
        # flycheck-info = {ul = {style = "wave"; color = "green";};};
        # flyspell-incorrect = {ul = {style = "wave"; color = "red";}; from = "unspecified";};

        # Highlight-indentation
        highlight-indentation-face = {bg = "surface1";};
        highlight-indentation-current-column-mode = {bg = "surface1";};

        # Rainbow delimiters
        rainbow-delimiters-depth-1-face = {fg = "blue";};
        rainbow-delimiters-depth-2-face = {fg = "pink";};
        rainbow-delimiters-depth-3-face = {fg = "green";};
        rainbow-delimiters-depth-4-face = {fg = "sand";};
        rainbow-delimiters-depth-5-face = {fg = "purple";};
        rainbow-delimiters-depth-6-face = {fg = "yellow";};
        rainbow-delimiters-depth-7-face = {fg = "cyan";};
        rainbow-delimiters-unmatched-face = {
          fg = "overlay0";
          bold = true;
          inverse-video = true;
        };

        # Bash
        sh-heredoc = {fg = "violet";};
        sh-quoted-exec = {fg = "violet";};

        # vterm
        vterm = {
          fg = "text";
          bg = "base";
        };
        vterm-color-default = {from = "default";};
        vterm-color-black = {
          fg = "crust";
          bg = "overlay1";
        };
        vterm-color-red = {
          fg = "red";
          bg = "ruby";
        };
        vterm-color-green = {
          fg = "green";
          bg = "viridis";
        };
        vterm-color-yellow = {
          fg = "yellow";
          bg = "sand";
        };
        vterm-color-blue = {
          fg = "blue";
          bg = "indigo";
        };
        vterm-color-magenta = {
          fg = "pink";
          bg = "magenta";
        };
        vterm-color-cyan = {
          fg = "teal";
          bg = "cyan";
        };
        vterm-color-white = {
          fg = "subtext0";
          bg = "white";
        };

        # message
        message-header-name = {
          fg = "blue";
          bold = true;
        };
        message-header-xheader = {fg = "teal";};
        message-header-newsgroups = {
          fg = "yellow";
          bold = true;
        };
        message-header-to = {
          fg = "green";
          bold = true;
        };
        message-header-cc = {fg = "green";};
        message-header-subject = {fg = "sand";};
        message-header-other = {fg = "pink";};
        message-mml = {fg = "cyan";};
        message-separator = {fg = "overlay0";};
        message-cited-text-1 = {fg = "violet";};
        message-cited-text-2 = {fg = "purple";};
        message-cited-text-3 = {fg = "ocean";};
        message-cited-text-4 = {fg = "overlay0";};

        # gnus
        gnus-header-name = {
          fg = "blue";
          bold = true;
        };
        gnus-header-from = {
          fg = "green";
          bold = true;
        };
        gnus-header-subject = {fg = "sand";};
        gnus-header-content = {fg = "pink";};
        gnus-header-newsgroups = {fg = "yellow";};
        gnus-cite-1 = {fg = "violet";};
        gnus-cite-2 = {fg = "purple";};
        gnus-cite-3 = {fg = "ocean";};
        gnus-cite-4 = {fg = "overlay0";};
        gnus-cite-5 = {fg = "violet";};
        gnus-cite-6 = {fg = "purple";};
        gnus-cite-7 = {fg = "indigo";};
        gnus-cite-8 = {fg = "overlay0";};
        gnus-cite-9 = {fg = "violet";};
        gnus-cite-10 = {fg = "purple";};
        gnus-cite-11 = {fg = "blue";};
        gnus-signature = {fg = "cyan";};

        # mu4e
        mu4e-header-face = {from = "default";};
        mu4e-title-face = {
          fg = "sand";
          bold = true;
        };
        mu4e-highlight-face = {
          fg = "viridis";
          bold = true;
        };
        mu4e-header-title-face = {fg = "pink";};
        mu4e-header-key-face = {
          fg = "blue";
          bold = true;
        };
        mu4e-header-highlight-face = {from = "highlight";};
        mu4e-header-value-face = {fg = "pink";};
        mu4e-special-header-value-face = {fg = "sand";};
        mu4e-contact-face = {fg = "green";};
        mu4e-compose-header-face = {
          fg = "green";
          bold = true;
        };
        mu4e-attach-number-face = {
          fg = "cyan";
          bold = true;
        };
        mu4e-footer-face = {fg = "cyan";};
        mu4e-compose-separator-face = {fg = "overlay0";};
        mu4e-context-face = {
          fg = "blue";
          bold = true;
        };
        mu4e-unread-face = {
          fg = "blue";
          bold = true;
        };
        mu4e-forwarded-face = {fg = "purple";};
        mu4e-draft-face = {fg = "pink";};
        mu4e-replied-face = {fg = "viridis";};
        mu4e-flagged-face = {fg = "sand";};
        mu4e-header-marks-face = {fg = "cyan";};
        mu4e-region-code = {fg = "violet";};
        mu4e-link-face = {from = "link";};
        mu4e-cited-1-face = {fg = "violet";};
        mu4e-cited-2-face = {fg = "purple";};
        mu4e-cited-3-face = {fg = "ocean";};
        mu4e-cited-4-face = {fg = "overlay0";};
        mu4e-cited-5-face = {fg = "violet";};
        mu4e-cited-6-face = {fg = "purple";};
        mu4e-cited-7-face = {fg = "indigo";};
      };
    };
  };
  config = mkIf cfg.enable {
    home.file = {
      ".emacs.d/${cfg.name}-theme.el".text = let
        mapDefCustoms = attrs:
          concatStringsSep "\n\n" (
            attrValues (mapAttrs (
                name: value: ''
                  (defcustom ${name} ${optionalString (hasAttr "default" value) value.default}
                     "${optionalString (hasAttr "description" value) value.description}"
                     ${optionalString (hasAttr "tag" value) ":tag \"${value.tag}\""}
                     ${optionalString (hasAttr "options" value) ":options '(${value.options})"}
                     ${optionalString (hasAttr "type" value) ":type '${value.type}"}
                     ${":group '${
                    if (hasAttr "group" value)
                    then value.group
                    else cfg.name
                  }"})''
              )
              attrs)
          );
        mapDefVars = attrs:
          concatStringsSep "\n\n" (
            attrValues (mapAttrs (
                name: value: ''
                  (defvar ${name} ${optionalString (hasAttr "default" value) value.default}
                    "${optionalString (hasAttr "description" value) value.description}")''
              )
              attrs)
          );
        mapDeFuns = attrs:
          concatStringsSep "\n" (
            attrValues (mapAttrs (
                name: value: ''
                  (defun ${name} (${optionalString (hasAttr "args" value) value.args})
                     "${optionalString (hasAttr "description" value) value.description}"
                      ${optionalString (hasAttr "interactive" value) "${
                    if (isBool value.interactive)
                    then "(interactive)"
                    else "(interactive ${value.interactive})"
                  }"}
                      ${optionalString (hasAttr "body" value) value.body})''
              )
              attrs)
          );
        mapPalette = attrs: let
          formattedAttrs =
            mapAttrsToList (
              name: value: "(${name} . \"#${value}\")"
            )
            attrs;
        in "(${concatStringsSep "\n" formattedAttrs})";
        mapCustomPalettes = attrs:
          mapDefCustoms (
            mapAttrs' (name: value:
              nameValuePair "${cfg.name}-${name}-colors" {
                tag = "${name} colors";
                description = "Colors used for ${cfg.name} ${name}";
                default = "'${mapPalette value}";
                type = "(alist :key-type symbol :value-type string)";
                options = "${concatStringsSep " " (attrNames value)}";
              })
            attrs
          );
        mapPaletteColors = attrs: let
          formattedAttrs =
            mapAttrsToList (
              name: value: "(${cfg.name}-${name}   \t\t(${cfg.name}/get-color '${name}) (${cfg.name}-quantize-color (${cfg.name}/get-color '${name})))"
            )
            attrs;
        in "'((undef        \"#ff00ff\" \"$ff00ff\")\n${concatStringsSep "\n" formattedAttrs})";
        mapFaces = attrs: let
          formattedAttrs =
            mapAttrsToList (
              name: value: "(${name}   \t\t${optionalString (hasAttr "bg" value) " :background ,${cfg.name}-${value.bg}"}${optionalString (hasAttr "fg" value) " :foreground ,${cfg.name}-${value.fg}"}${optionalString (hasAttr "from" value) " :inherit ${value.from}"}${optionalString (hasAttr "ul" value) " :underline (:color ,${cfg.name}-${value.ul.color} :style ${value.ul.style})"}${optionalString (hasAttr "bold" value) " :bold ${
                if value.bold
                then "t"
                else "nil"
              }"}${optionalString (hasAttr "height" value) " :height ${value.height}"}${optionalString ((hasAttr "nil" value) && value.nil) "nil"}${optionalString (hasAttr "inverse-video" value) " :inverse-video ${
                if value.inverse-video
                then "t"
                else "nil"
              }"}${optionalString (hasAttr "style" value) " :style ${value.style}"})"
            )
            attrs;
        in "'(${concatStringsSep "\n" formattedAttrs})";
      in
        /*
        elisp
        */
        ''
          ${optionalString ((builtins.stringLength cfg.fileHeader) > 0) cfg.fileHeader}
          (deftheme ${cfg.name})
          ;;; Configuration options
          (defgroup ${cfg.name} nil
            "Options for the ${cfg.name} theme.

          The theme has to be reloaded after changing anything in this group."
            :group 'faces)

          ${mapDefCustoms cfg.defCustoms}


          ${mapDefVars cfg.defVars}


          ;;; Theme flavor color palettes
          ${mapCustomPalettes cfg.themePalettes}


          ;;; Internal functions
          ${mapDeFuns cfg.deFuns}

          ;;; Theme definition
          (let (
             (colors
                ${mapPaletteColors cfg.themePalettes.dark})
             (faces
                ${mapFaces cfg.themeFaces}))

             (apply #'custom-theme-set-faces
                    '${cfg.name}
                    (let* ((expand-with-func
                            (lambda (func spec)
                              (let (reduced-color-list)
                                (dolist (col colors reduced-color-list)
                                  (push (list (car col) (funcall func col))
                                        reduced-color-list))
                                (eval `(let ,reduced-color-list
                                         (backquote ,spec))))))
                           whole-theme)
                      (pcase-dolist (`(,face . ,spec) faces)
                        (push `(,face
                                ((((min-colors 16777216)) ; fully graphical envs
                                  ,(funcall expand-with-func 'cadr spec))
                                (((min-colors 256))     ; 256 color terminals
                                  ,(if ${cfg.name}-use-24-bit-colors-on-256-colors-terms
                                       (funcall expand-with-func 'cadr spec)
                                     (funcall expand-with-func 'caddr spec)))
                                (t                      ; should be only tty-like envs
                                  ,(funcall expand-with-func 'cadddr spec))))
                              whole-theme))
                      whole-theme))
          )

          ;;;###autoload
          (when load-file-name
            (add-to-list 'custom-theme-load-path
                         (file-name-as-directory (file-name-directory load-file-name))))

          (provide-theme '${cfg.name})

          ;; Local Variables:
          ;; indent-tabs-mode: nil
          ;; End:

          ;;; ${cfg.name}-theme.el ends here
        '';
    };
  };
}
