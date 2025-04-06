{...}: {
  programs.tmux = {
    enable = true;
    clock24 = true;
    historyLimit = 8132;
    mouse = true;
    prefix = "C-z";
    extraConfig = ''
      # Command prompt
      bind -n M-x command-prompt

      # Session control
      bind N new-session

      # Window control
      unbind n
      unbind p
      unbind left
      unbind right
      unbind k
      bind n new-window
      bind -r left previous-window
      bind -r right next-window
      bind k kill-window

      # Pane control
      unbind 0
      unbind 1
      unbind 2
      unbind 3
      unbind \'
      unbind x
      unbind \"
      unbind %
      bind 0 kill-pane
      bind 1 kill-pane -a
      bind 2 split-window -v
      bind 3 split-window -h
      bind -r \' select-pane -t :.+
      bind -n M-\' select-pane -t :.+
      bind -r H resize-pane -L 2
      bind -r J resize-pane -D 2
      bind -r K resize-pane -U 2
      bind -r L resize-pane -R 2

      # Activity
      set -g monitor-activity on
      set -g visual-activity off

      # Copy mode
      setw -g mode-style 'fg=black bg=red bold'

      # Clock mode
      setw -g clock-mode-colour yellow

      # Statusbar
      set -g status-position bottom
      set -g status-justify left
      set -g status-style 'fg=red'
      set -g status-left '''
      set -g status-left-length 10
      set -g status-right-style 'fg=yellow'
      set -g status-right '%Y-%m-%d %H:%M '
      set -g status-right-length 50
      setw -g window-status-current-style 'fg=black bg=green'
      setw -g window-status-current-format ' #I #W #F '
      setw -g window-status-style 'fg=red bg=black'
      setw -g window-status-format ' #I #[fg=white]#W #[fg=yellow]#F '
      setw -g window-status-bell-style 'fg=yellow bg=red bold'
    '';
  };
}
