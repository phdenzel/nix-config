{...}: {
  programs.emacs.init = {
    globalFunctions = {
      "phd/relinum/on" = {
        description = "Activate relative line number in the current buffer (if show-line-numbers-mode is active).";
        interactive = true;
        body = "(setq display-line-numbers 'relative)";
      };
      "phd/relinum/off" = {
        description = "Deactivate relative line number in the current buffer (if show-line-numbers-mode is active).";
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
          (let ((fill-column 90002000))
          (fill-paragraph nil))'';
      };
      "phd/unfill-region" = {
        args = "start end";
        description = ''
          Replace newline chars in region from START to END by single spaces.
          This command does the reverse of `fill-region'.'';
        interactive = "r";
        body = ''
          (let ((fill-column 90002000))
          (fill-paragraph nil))'';
      };
      "phd/indent-entire-buffer" = {
        args = "&optional arg";
        description = "Indent the whole buffer. ARG for compatibility.";
        interactive = "P";
        body = ''
          (delete-trailing-whitespace)
          (indent-region (point-min) (point-max) nil)
          (untabify (point-min) (point-max))'';
      };
    };
  };
}
