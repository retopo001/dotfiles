# Project session layout template
# Usage: tmuxifier load-session project
# Customize PROJECT_DIR below for your project

session_root "~/signal-assembly-platform"

if initialize_session "project"; then

  # Main window: nvim with project
  new_window "editor"
  run_cmd "nvim ."

  # Terminal window with splits for running servers/commands
  new_window "servers"
  split_h 50

  # Git window
  new_window "git"
  run_cmd "git status"

  # Start on editor
  select_window 1

fi

finalize_and_go_to_session
