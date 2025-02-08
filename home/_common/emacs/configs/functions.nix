{...}: {
  programs.emacs.init = {
    globalFunctions = {
      "phd/relinum/on" = {
        description = "Activate relative line number in the current buffer.";
        interactive = true;
        body = "(setq display-line-numbers 'relative)";
      };
      "phd/relinum/off" = {
        description = "Deactivate relative line number in the current buffer.";
        interactive = true;
        body = "(setq display-line-numbers 1)";
      };
      "phd/vsplit-current-buffer" = {
        description = "Split the current buffer vertically and switch to the next.";
        interactive = true;
        body = ''
          (split-window-vertically)
          (other-window 1 nil)
          (switch-to-next-buffer)'';
      };
      "phd/hsplit-current-buffer" = {
        description = "Split the current buffer vertically and switch to the next.";
        interactive = true;
        body = ''
          (split-window-horizontally)
          (other-window 1 nil)
          (switch-to-next-buffer)'';
      };
      "phd/nuke-all-buffers" = {
        description = "Kill all buffers.";
        interactive = true;
        body = ''
          (mapc 'kill-buffer (buffer-list))
          (delete-other-windows)'';
      };
      "phd/unfill-paragraph" = {
        description = ''
          Replace newline chars in current paragraph by single spaces.
          This command does the reverse of `fill-paragraph'.'';
        interactive = true;
        body = ''
          (let ((fill-column most-positive-fixnum))
          (call-interactively 'fill-paragraph))'';
      };
      "phd/unfill-region" = {
        args = "start end";
        description = ''
          Replace newline chars in region from START to END by single spaces.
          This command does the reverse of `fill-region'.'';
        interactive = "\"r\"";
        body = ''
          (let ((fill-column most-positive-fixnum))
          (fill-region start end))'';
      };
      "phd/unfill-toggle" = {
        description = ''
          Toggle filling/unfilling of the current region.
          Operates on the current paragraph if no region is active.'';
        interactive = true;
        body = ''
          (let (deactivate-mark
                 (fill-column
                   (if (eq last-command this-command)
                       (progn (setq this-command nil)
                              most-positive-fixnum)
                     fill-column)))
             (call-interactively 'fill-paragraph))'';
      };
      "phd/indent-entire-buffer" = {
        description = "Indent the whole buffer.";
        interactive = true;
        body = ''
          (delete-trailing-whitespace)
          (indent-region (point-min) (point-max) nil)
          (untabify (point-min) (point-max))'';
      };
      "phd/hydra-hint-toggle" = {
        args = "name body";
        interative = "\"P\"";
        body = ''
          (let ((cv (hydra-get-property name :verbosity)))
            (if (eq cv 2)
                (hydra-set-property name :verbosity 0)
              (hydra-set-property name :verbosity 2))
            body)
        '';
      };
      "phd/ivy-bibtex-my-publications" = {
        args = "&optional arg";
        description = "Search BibTeX entries authored by Philipp Denzel";
        interactive = "\"P\"";
        body = ''
          (when arg (bibtex-completion-clear-cache))
          (bibtex-completion-init)
          (ivy-read "BibTeX Items: "
                    (bibtex-completion-candidates)
                    :initial-input "Philipp Denzel"
                    :caller 'ivy-bibtex
                    :action ivy-bibtex-default-action)'';
      };
      "phd/bibtex-entry-from-arxiv-doi" = {
        args = "doi &optional bibfile";
        description = "Insert and format bibtex entry from an arxiv DOI";
        interactive = "(list (read-string \"DOI: \" (doi-utils-maybe-doi-from-region-or-current-kill)))";
        body = ''
          (unless bibfile
            (setq bibfile (completing-read "Bibfile: " (org-ref-possible-bibfiles))))
          ;; filter DOI for arxiv id
          (setq arxiv-id-from-doi
                (replace-regexp-in-string "[aA][rR][xX][iI][vV][.:]" ""
                                          (car (last (split-string doi "/")))))
          ;; add bibtex entry by arXiv ID if DOI is from arXiv, else by DOI
          (if (and (string-match-p "[aA][rR][xX][iI][vV][.:]" doi)
                  (not (null arxiv-id-from-doi)))
              (save-window-excursion
                (with-current-buffer
                    (find-file-noselect bibfile)
                  (goto-char (point-min))
                  (if (re-search-forward (concat arxiv-id-from-doi "\\_>") nil t)
                      (message "arxiv:%s is already in this file" arxiv-id-from-doi)
                    (goto-char (point-max))
                    (when (not (looking-back "\n\n" (min 3 (point))))
                      (insert "\n\n"))
                    (arxiv-add-bibtex-entry arxiv-id-from-doi bibfile)
                    (save-buffer))))
            (doi-add-bibtex-entry doi bibfile))
        '';
      };
    };
  };
}
