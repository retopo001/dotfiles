#!/bin/bash
# Dotfiles installer for Arch Linux
# Uses GNU Stow for symlink management

set -e

echo "=== Dotfiles Installer (stow) ==="

DOTFILES_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKUP_DIR="$HOME/.dotfiles_backup_$(date +%Y%m%d_%H%M%S)"

# Check for stow
if ! command -v stow >/dev/null 2>&1; then
  echo "Installing stow..."
  if command -v pacman >/dev/null 2>&1; then
    sudo pacman -S --noconfirm stow
  elif command -v apt >/dev/null 2>&1; then
    sudo apt install -y stow
  else
    echo "ERROR: Please install GNU stow manually"
    exit 1
  fi
fi
echo "✓ stow available"

# Install Go if not present (required for gopls LSP server)
echo ""
echo "Checking for Go..."
if ! command -v go >/dev/null 2>&1; then
  echo "Installing Go..."
  if command -v pacman >/dev/null 2>&1; then
    sudo pacman -S --noconfirm go
  elif command -v apt >/dev/null 2>&1; then
    sudo apt install -y golang-go
  fi
fi
[ command -v go >/dev/null 2>&1 ] && echo "✓ Go: $(go version)"

# Install tmuxifier
echo ""
if [ ! -d "$HOME/.tmuxifier" ]; then
  echo "Installing tmuxifier..."
  git clone https://github.com/jimeh/tmuxifier.git ~/.tmuxifier 2>/dev/null && echo "✓ tmuxifier installed"
else
  echo "✓ tmuxifier already installed"
fi

# Install TPM (Tmux Plugin Manager)
if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
  echo "Installing TPM..."
  mkdir -p ~/.tmux/plugins
  git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm 2>/dev/null && echo "✓ TPM installed"
else
  echo "✓ TPM already installed"
fi

# Backup existing configs that would conflict
echo ""
echo "Checking for existing configs..."
NEEDS_BACKUP=false

check_and_backup() {
  local target="$HOME/$1"
  if [ -e "$target" ] && [ ! -L "$target" ]; then
    mkdir -p "$BACKUP_DIR/$(dirname "$1")"
    mv "$target" "$BACKUP_DIR/$1"
    echo "  Backed up: $1"
    NEEDS_BACKUP=true
  elif [ -L "$target" ]; then
    # Remove existing symlink (will be recreated by stow)
    rm "$target"
  fi
}

# Check files that stow will manage
while IFS= read -r -d '' file; do
  rel_path="${file#$DOTFILES_DIR/}"
  check_and_backup "$rel_path"
done < <(find "$DOTFILES_DIR" -type f \
  ! -path "$DOTFILES_DIR/.git/*" \
  ! -path "$DOTFILES_DIR/.git-crypt/*" \
  ! -name "install.sh" \
  ! -name "install-fonts.ps1" \
  ! -name "README.md" \
  ! -name "packages.txt" \
  ! -name ".gitignore" \
  ! -name ".gitattributes" \
  ! -name ".editorconfig" \
  ! -name "*.code-workspace" \
  ! -path "$DOTFILES_DIR/archlinux/*" \
  ! -path "$DOTFILES_DIR/windows/*" \
  ! -path "$DOTFILES_DIR/docs/*" \
  ! -path "$DOTFILES_DIR/omarchy-research/*" \
  -print0)

if [ "$NEEDS_BACKUP" = true ]; then
  echo "✓ Backed up existing configs to $BACKUP_DIR"
else
  echo "✓ No conflicts found"
fi

# Create parent directories that stow needs
echo ""
echo "Creating directory structure..."
mkdir -p ~/.config ~/.local/bin ~/.emacs.d

# Run stow
echo ""
echo "Stowing dotfiles..."
cd "$DOTFILES_DIR"

# Stow with ignore patterns
stow -v -t "$HOME" \
  --ignore='install\.sh' \
  --ignore='install-fonts\.ps1' \
  --ignore='README\.md' \
  --ignore='packages\.txt' \
  --ignore='\.gitignore' \
  --ignore='\.gitattributes' \
  --ignore='\.editorconfig' \
  --ignore='.*\.code-workspace' \
  --ignore='archlinux' \
  --ignore='windows' \
  --ignore='docs' \
  --ignore='omarchy-research' \
  .

echo "✓ Dotfiles stowed"

# Enable systemd user services
echo ""
echo "Enabling systemd user services..."
if [ -f ~/.config/systemd/user/emacs.service ]; then
  systemctl --user daemon-reload
  systemctl --user enable emacs.service 2>/dev/null && echo "✓ Enabled emacs.service"
fi

echo ""
echo "=== Installation Complete ==="
echo ""
echo "Symlinks created from ~/dotfiles to ~"
echo ""
echo "To update configs: edit files in ~/dotfiles, changes apply immediately"
echo "To add new configs: add to ~/dotfiles, run 'stow .' or re-run install.sh"
echo "To remove symlinks: run 'stow -D .' from ~/dotfiles"
echo ""
echo "Next steps:"
echo "1. Log out and back in (for systemd services)"
echo "2. Run 'exec zsh' or 'exec bash' to reload shell"
echo "3. Launch nvim and wait for plugins to install"
