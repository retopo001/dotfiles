# Dotfiles

Arch Linux / X11 / i3 development environment with vanilla Emacs, Neovim, and Tokyo Night theme.

**Management**: GNU Stow (symlinks). Edit files in `~/dotfiles`, changes apply immediately.

## Quick Start

```bash
git clone https://github.com/brian-wijaya/dotfiles.git ~/dotfiles
cd ~/dotfiles
./install.sh
```

The install script will:
- Install stow if needed
- Backup any conflicting configs to `~/.dotfiles_backup_*/`
- Create symlinks from `~/dotfiles` to `~`
- Enable systemd user services (Emacs daemon)

## Structure

```
dotfiles/
├── .config/
│   ├── i3/              # Window manager
│   ├── polybar/         # Status bar
│   ├── rofi/            # App launcher
│   ├── picom/           # Compositor
│   ├── dunst/           # Notifications
│   ├── nvim/            # Neovim (NvChad)
│   ├── ghostty/         # Terminal (default)
│   ├── alacritty/       # Terminal (alt)
│   ├── fish/            # Fish shell
│   ├── tmux/            # Tmux
│   ├── systemd/user/    # User services
│   │   └── emacs.service
│   └── wallpaper/
├── .emacs.d/            # Vanilla Emacs
│   ├── init.el          # Main config
│   └── cheatsheet.org   # Keybind reference
├── .claude/             # Claude Code CLI
│   ├── settings.json
│   └── mcp.json
├── bin/                 # Scripts (~200 lines each)
├── .bashrc
├── .zshrc
├── .zshenv
├── archlinux/packages.txt
└── install.sh
```

## Dotfile Management

Uses [GNU Stow](https://www.gnu.org/software/stow/) for symlink management.

```bash
# Edit any config - changes apply immediately (it's a symlink)
vim ~/dotfiles/.config/i3/config

# Add new config
mkdir -p ~/dotfiles/.config/newapp
cp ~/.config/newapp/config ~/dotfiles/.config/newapp/
cd ~/dotfiles && stow -R .

# Re-stow everything (after git pull, etc.)
cd ~/dotfiles && stow -R .

# Remove all symlinks
cd ~/dotfiles && stow -D .

# See what stow would do (dry run)
cd ~/dotfiles && stow -n -v .
```

## Secrets (git-crypt)

API keys and sensitive configs are stored directly in files but **encrypted at rest** via [git-crypt](https://github.com/AGWA/git-crypt). Files appear as binary gibberish in the repo until unlocked.

**Encrypted files** (see `.gitattributes`):
- `.emacs.d/init.el` - Contains Anthropic API key for gptel
- `.claude/settings.json` - Claude Code settings

```bash
# First-time setup (after clone)
git-crypt unlock  # Requires GPG key already added as collaborator

# Check encryption status
git-crypt status

# Add a collaborator
git-crypt add-gpg-user <GPG_KEY_ID>
```

If you clone this repo without unlocking, encrypted files will be binary. The configs won't work until you run `git-crypt unlock` with an authorized GPG key.

## Emacs

**Vanilla Emacs** (no Doom/Spacemacs). Config at `~/.emacs.d/init.el`.

Features:
- gptel (Claude/OpenAI LLM client)
- eglot (built-in LSP)
- vertico/corfu/marginalia (completion)
- tree-sitter (syntax highlighting)
- devdocs (offline documentation)
- magit (git)

### Daemon (systemd)

Emacs runs as a **systemd user service** for instant startup:

```bash
# Status
systemctl --user status emacs

# Restart (picks up init.el changes)
systemctl --user restart emacs

# View logs
journalctl --user -u emacs -f

# Open client
emacsclient -c      # New GUI frame
emacsclient -t      # Terminal frame
```

The service starts after `graphical-session.target` to ensure DISPLAY/XAUTHORITY are available.

### Key Bindings

| Key | Action |
|-----|--------|
| `C-c i` | Jump to function (imenu) |
| `C-c l` | Search buffer (consult-line) |
| `C-c r` | Search project (ripgrep) |
| `C-c f` | Find file in project |
| `C-c g` | Magit status |
| `C-c d` | Devdocs lookup |
| `C-c ?` | Open cheatsheet |
| `C-c c c` | Calendar (khal) |
| `C-x p f` | Project find file |

## i3 Window Manager

| Key | Action |
|-----|--------|
| `Super+Space` | App launcher (rofi) |
| `Super+Return` | Terminal (ghostty) |
| `Super+E` | Emacs (emacsclient) |
| `Super+W` | Close window |
| `Super+F` | Fullscreen |
| `Super+T` | Toggle floating |
| `Super+1-9` | Switch workspace |
| `Super+Shift+1-9` | Move to workspace |
| `Super+Arrow` | Focus direction |
| `Super+Shift+Arrow` | Move window |
| `Super+Escape` | System menu |
| `Super+BackSpace` | Toggle compositor |
| `Print` | Screenshot |
| `Pause` | Voice dictation |

## Shells

Fish (default), Zsh, Bash - all at feature parity:
- Starship prompt
- Zoxide (smart cd)
- Atuin (history sync)
- Fzf (fuzzy find)
- Eza (modern ls)

```bash
chsh -s /usr/bin/fish
```

## Terminals

- **ghostty** (default) - Fast, GPU-accelerated
- **alacritty** - Minimal
- **wezterm** - Feature-rich

All use JetBrains Mono Nerd Font + Tokyo Night theme.

## Installation Details

### Packages

```bash
# Arch packages
sudo pacman -S --needed - < ~/dotfiles/archlinux/packages.txt

# AUR
yay -S xidlehook clipmenu rofi-greenclip
```

### Claude Code

API key is stored in `.claude/settings.json` (git-crypt encrypted). After `git-crypt unlock`, it works automatically.

### Neovim

```bash
nvim  # Lazy.nvim auto-installs plugins
:MasonInstallAll
```

## Troubleshooting

**Emacs daemon not starting?**
```bash
systemctl --user status emacs
journalctl --user -u emacs --no-pager
```

**Symlinks broken after git pull?**
```bash
cd ~/dotfiles && stow -R .
```

**i3 config syntax error?**
```bash
i3 -C
```

**LSP not working?**
```bash
# Install language servers
sudo pacman -S typescript-language-server python-lsp-server rust-analyzer
go install golang.org/x/tools/gopls@latest
```

## Theme

Tokyo Night:
- Background: `#1a1b26`
- Foreground: `#c0caf5`
- Accent: `#7aa2f7`

## License

MIT
