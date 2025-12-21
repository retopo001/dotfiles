# Blank session - single zsh pane
# Usage: tmuxifier load-session blank

session_root "~"

if initialize_session "blank"; then

  new_window "shell"
  # Just zsh, no commands

fi

finalize_and_go_to_session
