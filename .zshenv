# ~/.zshenv - Runs for ALL zsh instances (login, interactive, non-interactive)
# Set PATH with ~/.local/bin FIRST
export PATH="$HOME/.local/bin:$PATH"

# XDG Base Directory Specification
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_STATE_HOME="$HOME/.local/state"

# Readline config (XDG location)
export INPUTRC="$XDG_CONFIG_HOME/readline/inputrc"

# Editor
export EDITOR="nvim"
export VISUAL="nvim"

# Terminal
export TERM="xterm-256color"
export LANG="en_US.UTF-8"

# Browser (for WSL)
export BROWSER="cmd.exe /C start"

# Go (for Mason's gopls installation)
# Mason uses 'go install' for gopls, so Go must be in PATH
if [ -d "/c/Program Files/Go/bin" ]; then
  export PATH="/c/Program Files/Go/bin:$PATH"
fi

# tmuxifier (tmux session templates)
# Install with: git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier
if [ -d "$HOME/.tmuxifier" ] && [ -f "$HOME/.tmuxifier/bin/tmuxifier" ]; then
  export PATH="$HOME/.tmuxifier/bin:$PATH"
fi

# GitHub Personal Access Token (for MCP server and other tools)
# Uses token from gh CLI config
if [ -f "$HOME/.config/gh/hosts.yml" ]; then
  export GITHUB_PERSONAL_ACCESS_TOKEN=$(grep -m1 'oauth_token:' "$HOME/.config/gh/hosts.yml" | awk '{print $2}')
fi
