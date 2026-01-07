# ~/.config/fish/config.fish

# Exit early if not interactive
status is-interactive; or exit

# --- IDE terminal detection (skip tmux) ---
if set -q CURSOR_AGENT; or set -q VSCODE_CWD; or set -q VSCODE_INJECTION; or set -q CURSOR_NO_TMUX; or test "$TERM_PROGRAM" = "vscode"
    set -gx CURSOR_NO_TMUX 1
end

# --- Starship prompt ---
starship init fish | source

# --- tmuxifier (tmux session templates) ---
if test -z "$CURSOR_NO_TMUX"; and test -d "$HOME/.tmuxifier"
    set -gx PATH $HOME/.tmuxifier/bin $PATH
    eval (tmuxifier init - fish 2>/dev/null)
end

# --- Aliases ---
# Note: Fish has built-in ls/grep functions that handle --color=auto automatically
alias ll 'ls -la'
alias v 'nvim'
alias cld 'claude --dangerously-skip-permissions'

# Emacs with Doom (Emacs 30+ requires --init-directory)
alias emacs 'emacs --init-directory=$HOME/.config/emacs'
alias e 'emacsclient --alternate-editor="emacs --init-directory=$HOME/.config/emacs" -c'

# --- Clipboard (Wayland - Hyprland) ---
alias pbcopy 'wl-copy'
alias pbpaste 'wl-paste'

# --- Environment ---
set -gx PATH $HOME/bin $HOME/.config/emacs/bin $PATH
set -gx GOPATH $HOME/go
set -gx PATH $GOPATH/bin $PATH
set -gx EMACSDIR $HOME/.config/emacs

# --- zoxide (smart cd) ---
zoxide init fish | source

# --- atuin (shell history) ---
atuin init fish | source

# --- fzf keybindings ---
# Fish has fzf integration via fzf.fish plugin or built-in
if test -f /usr/share/fish/vendor_functions.d/fzf_key_bindings.fish
    source /usr/share/fish/vendor_functions.d/fzf_key_bindings.fish
    fzf_key_bindings
end

# --- Auto-start tmux ---
# Skip in IDE terminals, Emacs, or if already in tmux
if not set -q TMUX; and not set -q CURSOR_NO_TMUX; and not set -q WEZTERM_NOTMUX; and not set -q INSIDE_EMACS
    tmux attach -t main 2>/dev/null; or tmux new -s main -c ~
end
