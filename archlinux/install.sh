#!/bin/bash
# Arch Linux X11/i3 Dotfiles Installer

set -e

echo "=== Arch Linux Dotfiles Installer ==="

DOTFILES_DIR="$(cd "$(dirname "$0")/.." && pwd)"
BACKUP_DIR="$HOME/.config_backup_$(date +%Y%m%d_%H%M%S)"

# Check if running on Arch
if ! command -v pacman >/dev/null 2>&1; then
    echo "Error: This script is for Arch Linux only"
    exit 1
fi

# Install packages
echo ""
echo "Installing packages..."
if [ -f "$DOTFILES_DIR/archlinux/packages.txt" ]; then
    grep -v '^#' "$DOTFILES_DIR/archlinux/packages.txt" | grep -v '^$' | sudo pacman -S --needed -
    echo "✓ Pacman packages installed"
fi

# AUR packages (if yay is available)
if command -v yay >/dev/null 2>&1; then
    echo ""
    echo "Installing AUR packages..."
    yay -S --needed xidlehook i3lock-color clipmenu satty google-chrome impala bluetui pulsemixer sddm-theme-tokyo-night-git
    echo "✓ AUR packages installed"
else
    echo "⚠ yay not found, skipping AUR packages"
fi

# Backup existing configs
echo ""
echo "Backing up existing configs to $BACKUP_DIR..."
mkdir -p "$BACKUP_DIR"
for dir in i3 polybar rofi picom dunst alacritty fish; do
    [ -d "$HOME/.config/$dir" ] && cp -r "$HOME/.config/$dir" "$BACKUP_DIR/"
done
echo "✓ Backup complete"

# Create directories
echo ""
echo "Creating directories..."
mkdir -p ~/.config/{i3,polybar,rofi/themes,picom,dunst,alacritty,fish}
mkdir -p ~/bin

# Copy configs
echo ""
echo "Copying config files..."

# i3
[ -d "$DOTFILES_DIR/i3" ] && cp -r "$DOTFILES_DIR/i3/"* ~/.config/i3/ && echo "✓ i3"

# Polybar
[ -d "$DOTFILES_DIR/polybar" ] && cp -r "$DOTFILES_DIR/polybar/"* ~/.config/polybar/ && echo "✓ polybar"

# Rofi
[ -d "$DOTFILES_DIR/rofi" ] && cp -r "$DOTFILES_DIR/rofi/"* ~/.config/rofi/ && echo "✓ rofi"

# Picom
[ -d "$DOTFILES_DIR/picom" ] && cp -r "$DOTFILES_DIR/picom/"* ~/.config/picom/ && echo "✓ picom"

# Dunst
[ -d "$DOTFILES_DIR/dunst" ] && cp -r "$DOTFILES_DIR/dunst/"* ~/.config/dunst/ && echo "✓ dunst"

# Alacritty
[ -d "$DOTFILES_DIR/alacritty" ] && cp -r "$DOTFILES_DIR/alacritty/"* ~/.config/alacritty/ && echo "✓ alacritty"

# Fish
[ -d "$DOTFILES_DIR/fish" ] && cp -r "$DOTFILES_DIR/fish/"* ~/.config/fish/ && echo "✓ fish"

# Scripts
[ -d "$DOTFILES_DIR/bin" ] && cp "$DOTFILES_DIR/bin/"* ~/bin/ 2>/dev/null && chmod +x ~/bin/* && echo "✓ scripts"

# Make polybar launcher executable
chmod +x ~/.config/polybar/launch.sh 2>/dev/null

# Set fish as default shell
echo ""
echo "Setting fish as default shell..."
if [ "$SHELL" != "/usr/bin/fish" ]; then
    chsh -s /usr/bin/fish
    echo "✓ Fish set as default shell"
else
    echo "✓ Fish already default"
fi

# Configure SDDM
echo ""
echo "Configuring SDDM..."
sudo mkdir -p /etc/sddm.conf.d
echo -e "[Theme]\nCurrent=tokyo-night-sddm" | sudo tee /etc/sddm.conf.d/theme.conf >/dev/null
sudo systemctl enable sddm
echo "✓ SDDM configured"

echo ""
echo "=== Installation Complete ==="
echo ""
echo "Next steps:"
echo "1. Reboot: systemctl reboot"
echo "2. SDDM will handle login"
echo "3. i3 will start automatically"
echo ""
echo "See archlinux/SETUP.md for keybindings and configuration details."
