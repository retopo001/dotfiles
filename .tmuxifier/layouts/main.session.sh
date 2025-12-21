# Main development layout
# 3 columns: left (nvim/yazi stacked), middle (zsh), right (claude)
# Usage: tmuxifier load-session main

session_root "~"

if initialize_session "main"; then

  new_window "workspace"

  # Start with full pane, split into 3 columns
  # split_h 67 = left keeps 33%, right gets 67%
  split_h 67

  # Now on right pane (67%), split in half for middle and right
  split_h 50

  # Pane layout now: [0: left 33%] [1: middle 33%] [2: right 33%]

  # Right pane (2): claude
  select_pane 2
  run_cmd "claude --dangerously-skip-permissions"

  # Middle pane (1): zsh (default, no command needed)
  select_pane 1

  # Left pane (0): split vertically for nvim (top) and yazi (bottom)
  select_pane 0
  split_v 50

  # Pane layout now: [0: left-top] [1: middle] [2: right] [3: left-bottom]

  # Left-top (0): nvim
  select_pane 0
  run_cmd "nvim"

  # Left-bottom (3): yazi
  select_pane 3
  run_cmd "yazi"

  # Focus on middle pane
  select_pane 1

fi

finalize_and_go_to_session
