{
  programs.tmux = {
    enable = true;
    secureSocket = false;

    # Support all of st's features
    terminal = "st-256color";

    # Settings
    escapeTime = 0;
    clock24 = true;

    # Vi mode
    keyMode = "vi";
    shortcut = "t";

    extraConfig = ''
      # Support cursor shapes
      # https://github.com/neovim/neovim/wiki/FAQ/fe93b6f551589a55b1c34670fc64b2bd56547924#how-to-change-cursor-shape-in-the-terminal
      set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'

      set -g mouse on

      # Better windowing --- {{{

      # Bind Ctrl-w for window jumping
      set -g prefix2 "C-w"

      # Unbind previous keys
      unbind "%"
      unbind "\""

      # Split with current path
      bind "H" split-window -v -c "#{pane_current_path}"
      bind "V" split-window -h -c "#{pane_current_path}"
      bind "c" new-window -c "#{pane_current_path}"

      # Select panes with vi bindings
      bind "h" select-pane -L
      bind "j" select-pane -D
      bind "k" select-pane -U
      bind "l" select-pane -R

      # }}}

      # Better copy mode --- {{{

      # Escape opens
      bind -n C-g copy-mode

      # Disable tmux clipboard in favor of xclip which won't truncate the
      # region.
      set -g set-clipboard off

      # Select and copy
      bind -T copy-mode-vi "v" send -X begin-selection
      bind -T copy-mode-vi "y" send -X copy-pipe "xclip -sel clip -i > /dev/null"

      # Mouse does not end selection
      unbind -T copy-mode-vi "MouseDragEnd1Pane"

      # Insert mode buttons cancel selection mode
      bind -T copy-mode-vi "a" send -X cancel
      bind -T copy-mode-vi "i" send -X cancel

      # }}}
    '';
  };
}
