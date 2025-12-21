# Claude Code session layout
# Usage: tmuxifier load-session claude

session_root "~"

if initialize_session "claude"; then

  # Main window: Claude Code (wide left) + terminal (narrow right)
  new_window "claude"
  split_h 30
  select_pane 0
  run_cmd "claude"

  # Second window: nvim for editing
  new_window "nvim"
  run_cmd "nvim"

  # Third window: terminals for running commands
  new_window "run"
  split_h 50

  # Start on the claude window
  select_window 1

fi

finalize_and_go_to_session
