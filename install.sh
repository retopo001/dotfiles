#!/bin/bash
# Dotfiles installer for Arch Linux WSL (zsh environment)
# Uses direct file copies (no symlinks) for independent configs

set -e

echo "=== Dotfiles Installer ==="

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKUP_DIR="$HOME/.config_backup_$(date +%Y%m%d_%H%M%S)"

# Install Go if not present (required for gopls LSP server)
echo "Checking for Go installation..."
if ! command -v go >/dev/null 2>&1; then
  echo "Go not found. Installing Go..."

  if command -v pacman >/dev/null 2>&1; then
    echo "Detected Arch Linux. Installing Go via pacman..."
    sudo pacman -S --noconfirm go
  elif command -v apt >/dev/null 2>&1; then
    echo "Detected Debian/Ubuntu. Installing Go via apt..."
    sudo apt update
    sudo apt install -y golang-go
  else
    echo "No package manager found. Downloading Go binary..."
    GO_VERSION="1.23.4"
    GO_ARCH="amd64"
    GO_OS="linux"

    if [[ "$(uname -m)" == "aarch64" ]] || [[ "$(uname -m)" == "arm64" ]]; then
      GO_ARCH="arm64"
    fi

    GO_TAR="go${GO_VERSION}.${GO_OS}-${GO_ARCH}.tar.gz"
    GO_URL="https://go.dev/dl/${GO_TAR}"
    INSTALL_DIR="$HOME/.local"

    echo "Downloading Go ${GO_VERSION}..."
    cd /tmp
    curl -L -o "$GO_TAR" "$GO_URL"

    echo "Extracting Go..."
    rm -rf "$INSTALL_DIR/go"
    tar -C "$INSTALL_DIR" -xzf "$GO_TAR"

    # Add Go to PATH in .zshenv if not already there
    if ! grep -q "$INSTALL_DIR/go/bin" ~/.zshenv 2>/dev/null; then
      echo "" >> ~/.zshenv
      echo "# Go (for gopls LSP server)" >> ~/.zshenv
      echo "export PATH=\"$INSTALL_DIR/go/bin:\$PATH\"" >> ~/.zshenv
    fi

    export PATH="$INSTALL_DIR/go/bin:$PATH"
    echo "Go installed to $INSTALL_DIR/go"
  fi

  if command -v go >/dev/null 2>&1; then
    echo "✓ Go installed successfully: $(go version)"
  else
    echo "⚠ Warning: Go installation may have failed. Please install manually."
  fi
else
  echo "✓ Go already installed: $(go version)"
fi

# Install tmuxifier (tmux session templates)
echo ""
echo "Installing tmuxifier..."
if [ ! -d "$HOME/.tmuxifier" ]; then
  if git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier 2>/dev/null; then
    echo "✓ tmuxifier installed"
  else
    echo "⚠ Failed to install tmuxifier"
  fi
else
  echo "✓ tmuxifier already installed"
fi

# Install TPM (Tmux Plugin Manager)
echo ""
echo "Installing TPM (Tmux Plugin Manager)..."
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
  mkdir -p ~/.tmux/plugins
  if git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm 2>/dev/null; then
    echo "✓ TPM installed"
  else
    echo "⚠ Failed to install TPM"
  fi
else
  echo "✓ TPM already installed"
fi

# Backup existing configs
echo ""
echo "Backing up existing configs to $BACKUP_DIR..."
mkdir -p "$BACKUP_DIR"
[ -f ~/.zshenv ] && cp ~/.zshenv "$BACKUP_DIR/"
[ -f ~/.zprofile ] && cp ~/.zprofile "$BACKUP_DIR/"
[ -f ~/.zshrc ] && cp ~/.zshrc "$BACKUP_DIR/"
[ -f ~/.tmux.conf ] && cp ~/.tmux.conf "$BACKUP_DIR/"
[ -f ~/.wezterm.lua ] && cp ~/.wezterm.lua "$BACKUP_DIR/"
[ -d ~/.config/nvim ] && cp -r ~/.config/nvim "$BACKUP_DIR/"
[ -d ~/.config/tmux ] && cp -r ~/.config/tmux "$BACKUP_DIR/"
[ -d ~/.config/wezterm ] && cp -r ~/.config/wezterm "$BACKUP_DIR/"
[ -f ~/.config/starship.toml ] && cp ~/.config/starship.toml "$BACKUP_DIR/"
echo "✓ Backup complete"

# Create directory structure
echo ""
echo "Creating directory structure..."
mkdir -p ~/.config/nvim
mkdir -p ~/.config/tmux
mkdir -p ~/.config/wezterm
mkdir -p ~/.local/state/zsh

# Copy configs (direct copies, no symlinks)
echo ""
echo "Copying config files..."

# Neovim config
if [ -d "$DOTFILES_DIR/.config/nvim" ]; then
  rm -rf ~/.config/nvim
  cp -r "$DOTFILES_DIR/.config/nvim" ~/.config/nvim
  echo "✓ Copied nvim config"
fi

# Tmux config
if [ -f "$DOTFILES_DIR/.config/tmux/tmux.conf" ]; then
  cp "$DOTFILES_DIR/.config/tmux/tmux.conf" ~/.config/tmux/tmux.conf
  echo "✓ Copied tmux config"
fi

# WezTerm config (WSL)
if [ -f "$DOTFILES_DIR/.config/wezterm/wezterm.lua" ]; then
  cp "$DOTFILES_DIR/.config/wezterm/wezterm.lua" ~/.config/wezterm/wezterm.lua
  echo "✓ Copied wezterm config"
fi

# Starship config
if [ -f "$DOTFILES_DIR/.config/starship.toml" ]; then
  cp "$DOTFILES_DIR/.config/starship.toml" ~/.config/starship.toml
  echo "✓ Copied starship config"
fi

# Zsh configs (if they exist in dotfiles)
[ -f "$DOTFILES_DIR/.zshenv" ] && cp "$DOTFILES_DIR/.zshenv" ~/.zshenv && echo "✓ Copied .zshenv"
[ -f "$DOTFILES_DIR/.zprofile" ] && cp "$DOTFILES_DIR/.zprofile" ~/.zprofile && echo "✓ Copied .zprofile"
[ -f "$DOTFILES_DIR/.zshrc" ] && cp "$DOTFILES_DIR/.zshrc" ~/.zshrc && echo "✓ Copied .zshrc"

# Copy WezTerm config to Windows
echo ""
echo "Copying WezTerm config to Windows..."
WIN_USER=""
for user in wijay $(whoami 2>/dev/null) $(id -un 2>/dev/null); do
  if [ -d "/mnt/c/Users/$user" ]; then
    WIN_USER="$user"
    break
  fi
done

if [ -n "$WIN_USER" ] && [ -d "/mnt/c/Users/$WIN_USER" ]; then
  if cp "$DOTFILES_DIR/.config/wezterm/wezterm.lua" "/mnt/c/Users/$WIN_USER/.wezterm.lua" 2>/dev/null; then
    echo "✓ WezTerm config copied to /mnt/c/Users/$WIN_USER/.wezterm.lua"
  else
    echo "⚠ Could not copy WezTerm config to Windows"
  fi
else
  echo "⚠ Could not detect Windows username"
fi

# Install Nerd Fonts on Windows
echo ""
echo "Installing Nerd Fonts on Windows..."
if [ -n "$WIN_USER" ] && command -v powershell.exe >/dev/null 2>&1; then
  if [ -f "$DOTFILES_DIR/install-fonts.ps1" ]; then
    PS_SCRIPT_PATH=$(wslpath -w "$DOTFILES_DIR/install-fonts.ps1" 2>/dev/null)
    if [ -z "$PS_SCRIPT_PATH" ]; then
      PS_SCRIPT_PATH=$(echo "$DOTFILES_DIR/install-fonts.ps1" | sed 's|/mnt/c|C:|' | sed 's|/|\\|g')
    fi
    powershell.exe -ExecutionPolicy Bypass -File "$PS_SCRIPT_PATH" 2>&1 && echo "✓ Nerd Fonts installed" || echo "⚠ Font installation had issues"
  fi
fi

echo ""
echo "=== Installation Complete ==="
echo ""
echo "Next steps:"
echo "1. Restart WezTerm"
echo "2. Run 'exec zsh' if not already in zsh"
echo "3. Launch nvim and wait for plugins to install"
echo "4. Run :MasonInstallAll in Neovim"
echo "5. In tmux, press 'Prefix + I' to install tmux plugins"
echo ""
echo "Note: Configs are direct copies. To update from dotfiles, run this script again."
