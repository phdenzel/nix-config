{...}: {
  programs.emacs.init.usePackage = {
    multiple-cursors.enable = true;
    sudo-edit.enable = true;
    comment-dwim-2.enable = true;
    hungry-delete.enable = true;
    expand-region.enable = true;
    drag-stuff.enable = true;
    company = {
      enable = true;
      custom = {
        company-idle-delay = 0.3;
        company-minimum-prefix-length = 2;
        company-tooltip-align-annotations = true;
        company-tooltip-annotation-padding = 1;
        company-tooltip-margin = 1;
        company-detect-icons-margin-function = "'company-dot-icons-margin";
      };
      config = "(global-company-mode t)";
    };
    electric = {
      enable = true;
      commands = ["electric-indent-local-mode"];
      hook = [
        "(prog-mode . electric-pair-mode)"
        "(prog-mode . electric-indent-mode)"
        "(prog-mode . electric-layout-mode)"
      ];
    };
    yasnippet = {
      enable = true;
      hook = [
        "(org-mode . yas-minor-mode)"
        "(latex-mode . yas-minor-mode)"
        "(LaTeX-mode . yas-minor-mode)"
        "(python-mode . yas-minor-mode)"
      ];
      custom = {
        yas-snippet-dirs = "'((file-name-concat user-emacs-directory \"snippets\"))";
      };
    };
  };

  # Custom yasnippets
  home.file = {
    # Various typesetting modes
    ".emacs.d/snippets/org-mode/org-header".text = ''
      # -*- mode: snippet -*-
      # name: org-header
      # key: <hr
      # --
      #+AUTHOR: phdenzel
      #+TITLE: $1
      #+DATE: `(format-time-string "%Y-%m-%d %a")`
      #+OPTIONS: author:nil title:t date:nil timestamp:nil toc:nil num:nil \n:nil
      $0
    '';
    ".emacs.d/snippets/org-mode/src-src".text = ''
      # -*- mode: snippet -*-
      # name: src-src
      # key: <s
      # --
      #+begin_src $1
      $0
      #+end_src
    '';
    ".emacs.d/snippets/org-mode/el-src".text = ''
      # -*- mode: snippet -*-
      # name: el-src
      # key: <el
      # --
      #+begin_src emacs-lisp
      $0
      #+end_src
    '';
    ".emacs.d/snippets/org-mode/sh-src".text = ''
      # -*- mode: snippet -*-
      # name: sh-src
      # key: <sh
      # --
      #+begin_src sh
      $0
      #+end_src
    '';
    ".emacs.d/snippets/org-mode/bash-src".text = ''
      # -*- mode: snippet -*-
      # name: bash-src
      # key: <b
      # --
      #+begin_src bash
      $0
      #+end_src
    '';

    # LaTeX-mode
    ".emacs.d/snippets/LaTeX-mode/.yas-parents".text = ''
      org-mode latex-mode
    '';
    ".emacs.d/snippets/LaTeX-mode/equation".text = ''
      # -*- mode: snippet -*-
      # name: equation
      # key: <eq
      # --
      \begin{equation}
        $1
      \end{equation}
      $0
    '';
    ".emacs.d/snippets/LaTeX-mode/fraction".text = ''
      # -*- mode: snippet -*-
      # name: fraction
      # key: <fr
      # --
      \frac{$1}{$2}$0
    '';
    ".emacs.d/snippets/LaTeX-mode/parlr".text = ''
      # -*- mode: snippet -*-
      # name: parlr
      # key: <p
      # --
      \left($1\right)$0
    '';
    ".emacs.d/snippets/LaTeX-mode/tex-formula".text = ''
      # -*- mode: snippet -*-
      # name: tex-formula
      # key: <fta
      # --
      \$\$ $0 \$\$
    '';
    ".emacs.d/snippets/LaTeX-mode/tex-inline-formula".text = ''
      # -*- mode: snippet -*-
      # name: tex-inline-formula
      # key: <ft
      # --
      \$$0\$
    '';
    ".emacs.d/snippets/LaTeX-mode/formula".text = ''
      # -*- mode: snippet -*-
      # name: align-formula
      # key: <fa
      # --
      \\[$0\\]
    '';
    ".emacs.d/snippets/LaTeX-mode/inline-formula".text = ''
      # -*- mode: snippet -*-
      # name: inline-formula
      # key: <f
      # --
      \\($0\\)
    '';
    ".emacs.d/snippets/latex-mode/.yas-parents".text = ''
      org-mode LaTeX-mode
    '';

    # Python-mode
    ".emacs.d/snippets/python-mode/ifname".text = ''
      # -*- mode: snippet -*-
      # name: ifname
      # key: <if
      # --
      if __name__ == "__main__":
          $0
    '';
    ".emacs.d/snippets/python-mode/class".text = ''
      # -*- mode: snippet -*-
      # name: class
      # key: <cl
      # --
      class $1:
          """$2
          """
          def __init__(self,) -> None:
              """$3

              Args:

              """
              $0
    '';
    ".emacs.d/snippets/python-mode/function".text = ''
      # -*- mode: snippet -*-
      # name: function
      # key: <fn
      # --
      def $1($2):
          """$3

          Args:

          """
          $0
    '';
  };
}
