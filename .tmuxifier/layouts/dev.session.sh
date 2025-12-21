# Development session layout
# Usage: tmuxifier load-session dev

session_root "~"

if initialize_session "dev"; then

  # Main window: nvim (large) + terminal (small below)
  new_window "code"
  split_v 25
  select_pane 0
  run_cmd "nvim"

  # Second window: two terminals side by side
  new_window "terminals"
  split_h 50

  # Third window: single terminal for misc tasks
  new_window "misc"

  # Start on the code window
  select_window 1

fi

finalize_and_go_to_session
