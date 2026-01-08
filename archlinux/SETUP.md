# Arch Linux X11/i3 Setup Guide

Complete setup guide for Arch Linux with i3 window manager, X11, and dual-boot Windows 11.

## Hardware
- GPU: NVIDIA RTX 3090 Ti (nvidia-open driver)
- Dual-boot: Arch Linux + Windows 11

---

## 1. Base System

### Bootloader: Limine + systemd-boot

**Limine** (primary) - `/boot/limine.conf`:
```
timeout: 3
default_entry: 1
interface_branding: Arch Linux

term_background: 1a1b26
backdrop: 1a1b26
term_foreground: c0caf5
term_palette: 15161e;f7768e;9ece6a;e0af68;7aa2f7;bb9af7;7dcfff;a9b1d6

/Arch Linux
    protocol: linux
    path: boot():/vmlinuz-linux
    cmdline: initrd=/initramfs-linux.img root=PARTUUID=<your-partuuid> rw

/Windows 11
    protocol: efi
    path: boot():/EFI/Microsoft/Boot/bootmgfw.efi
```

**systemd-boot** (backup) - `/boot/loader/loader.conf`:
```
default arch
timeout 5
editor no
```

**EFI Boot Order** (Limine first):
```bash
sudo efibootmgr -o 0001,0002,0003,0000
```

---

## 2. Package Installation

### Pacman Packages
```bash
sudo pacman -S --needed \
  xorg-server xorg-xinit xorg-xrandr xorg-xsetroot xorg-xset \
  i3-wm picom polybar rofi dunst feh unclutter xclip maim slop \
  xdotool redshift alacritty \
  fish starship zoxide fd bat eza fzf \
  chromium nautilus \
  pamixer playerctl brightnessctl pipewire pipewire-pulse wireplumber \
  btop lazygit jq neovim \
  ttf-jetbrains-mono-nerd papirus-icon-theme \
  ffmpeg zenity gpick libnotify \
  sddm qt5-graphicaleffects qt5-quickcontrols2 \
  github-cli nodejs npm
```

### AUR Packages
```bash
yay -S --needed \
  xidlehook i3lock-color clipmenu satty \
  google-chrome impala bluetui pulsemixer \
  sddm-theme-tokyo-night-git
```

---

## 3. Display Manager: SDDM

**Install and enable:**
```bash
sudo systemctl enable sddm
```

**Theme config** - `/etc/sddm.conf.d/theme.conf`:
```ini
[Theme]
Current=tokyo-night-sddm
```

**Disable fish auto-startx** (SDDM handles X now):
Comment out startx in `~/.config/fish/config.fish`

---

## 4. Window Manager: i3

Key configs in `~/.config/i3/config`:

### Gaps and Borders
```
gaps inner 10
gaps outer 10
default_border pixel 2
for_window [class=".*"] border pixel 2
```

### Key Bindings
| Binding | Action |
|---------|--------|
| Super+Return | Terminal |
| Super+Space | App launcher (rofi) |
| Super+Alt+Space | Menu |
| Super+W | Close window |
| Super+1-4 | Workspaces |
| Super+H/J/K/L | Focus navigation |
| Super+Shift+H/J/K/L | Move windows |
| Super+Ctrl+L | Lock screen |
| Super+M | Logout |
| Print | Screenshot |

### Autostart
```
exec_always --no-startup-id xsetroot -solid "#1a1b26"
exec_always --no-startup-id pkill picom; sleep 0.5; picom -b
exec_always --no-startup-id feh --bg-fill ~/.config/wallpaper/current.jpg
exec_always --no-startup-id ~/.config/polybar/launch.sh
exec --no-startup-id dunst
exec --no-startup-id clipmenud
```

---

## 5. Status Bar: Polybar

Config at `~/.config/polybar/config.ini`

**Modules:**
- Left: i3 workspaces
- Center: Date/time
- Right: Volume, Memory, CPU, WiFi, Tray

**Click actions:**
- Volume → pulsemixer
- Memory/CPU → btop
- WiFi → impala
- Date → menu

---

## 6. App Launcher: Rofi

Config at `~/.config/rofi/config.rasi`

**Fix for rofi 2.0 drun mode:**
```css
drun {
    parse-user: true;
    parse-system: true;
    DBusActivatable: false;
}
```

Custom app-launcher script at `~/bin/app-launcher` for reliable execution.

---

## 7. Compositor: Picom

Config at `~/.config/picom/picom.conf`
- Shadows, transparency, blur
- Vsync enabled for NVIDIA

---

## 8. Shell: Fish

Config at `~/.config/fish/config.fish`
- Starship prompt
- Zoxide for directory jumping
- Clipboard aliases (pbcopy/pbpaste)

---

## 9. Tokyo Night Theme

Colors used throughout:
```
Background: #1a1b26
Foreground: #c0caf5
Accent/Border: #33ccff
Secondary: #7aa2f7
Alert: #f7768e
Muted: #565f89
```

---

## 10. Scripts (~/bin/)

| Script | Purpose |
|--------|---------|
| menu | Main rofi menu system |
| app-launcher | Rofi app launcher |
| screenshot | maim/slop/satty wrapper |
| lock-screen | i3lock-color wrapper |
| toggle-nightlight | Redshift toggle |
| toggle-topbar | Polybar toggle |
| pkg-install | fzf package installer |
| pkg-remove | fzf package remover |

---

## 11. Troubleshooting

### SDDM background persists after login
Add to i3 autostart:
```
exec_always --no-startup-id xsetroot -solid "#1a1b26"
```

### Rofi drun not launching apps
Use custom app-launcher script or disable DBusActivatable in rofi config.

### Chrome won't launch
Remove stale lock: `rm ~/.config/google-chrome/SingletonLock`

### Limine not booting
Check EFI boot order: `efibootmgr -v`
Set Limine first: `sudo efibootmgr -o 0001,...`

---

## Quick Install

```bash
# Clone dotfiles
git clone git@github.com:Retopo001/dotfiles.git ~/dotfiles
cd ~/dotfiles

# Run install script
./install.sh

# Enable SDDM
sudo systemctl enable sddm

# Reboot
systemctl reboot
```
