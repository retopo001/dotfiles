# Neovim Workflow Orientation Guide

Dense keybinding reference for your configuration. Every key listed should be pressed at least once.

**Legend — Where each mapping comes from:**
- `[.wezterm.lua]` = Your WezTerm config
- `[.tmux.conf]` = Your tmux config
- `[mappings.lua]` = Your Neovim mappings
- `[TMUX]` = Tmux default
- `[NVCHAD]` = NvChad default mapping
- `[NVIMTREE]` = NvimTree plugin default
- `[TELESCOPE]` = Telescope plugin default
- `[NEOVIM]` = Vanilla Neovim/Vim default
- `[FZF]` = FZF shell integration default
- `[ZOXIDE]` = Zoxide default

**Prefix** = `Ctrl+Space` (your tmux prefix, defined in .tmux.conf)
**Leader** = `Space` (NvChad default)

---

## STAGE 1: Terminal & Tmux

Your terminal auto-starts tmux when you open WezTerm (configured in .bashrc).

### WezTerm Tab Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Shift+T` | [.wezterm.lua] | New bash tab (bypasses tmux) |
| `Ctrl+Shift+N` | [.wezterm.lua] | New Neovim tab (bypasses tmux) |

### Tmux Session Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+Space` | [.tmux.conf] | Tmux prefix (replaces default `Ctrl+b`) |
| `Prefix d` | [TMUX] | Detach from session (exit without closing) |
| `Prefix s` | [TMUX] | List/switch sessions |
| `Prefix $` | [TMUX] | Rename current session |
| `Prefix (` | [TMUX] | Switch to previous session |
| `Prefix )` | [TMUX] | Switch to next session |

**Shell command:** `tmux attach -t main` - Reattach after detach

### Tmux Window Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix c` | [.tmux.conf] | Create new window (rebind of tmux default) |
| `Prefix &` | [.tmux.conf] | Kill current window (rebind of tmux default) |
| `Prefix 1` | [TMUX] | Switch to window 1 |
| `Prefix 2` | [TMUX] | Switch to window 2 |
| `Prefix n` | [TMUX] | Next window |
| `Prefix p` | [TMUX] | Previous window |
| `Prefix w` | [TMUX] | List windows (interactive picker) |
| `Prefix ,` | [TMUX] | Rename current window |

### Tmux Pane Management

| Keys | Source | Action |
|------|--------|--------|
| `Prefix \|` | [.tmux.conf] | Split vertically (replaces default `%`) |
| `Prefix -` | [.tmux.conf] | Split horizontally (replaces default `"`) |
| `Prefix h` | [.tmux.conf] | Move to left pane (custom vim-style) |
| `Prefix j` | [.tmux.conf] | Move to pane below (custom vim-style) |
| `Prefix k` | [.tmux.conf] | Move to pane above (custom vim-style) |
| `Prefix l` | [.tmux.conf] | Move to right pane (custom vim-style) |
| `Prefix x` | [.tmux.conf] | Kill current pane (rebind of tmux default) |
| `Prefix <` | [.tmux.conf] | Resize pane left 5 units (repeatable) |
| `Prefix >` | [.tmux.conf] | Resize pane right 5 units (repeatable) |
| `Prefix +` | [.tmux.conf] | Resize pane up 2 units (repeatable) |
| `Prefix =` | [.tmux.conf] | Resize pane down 2 units (repeatable) |
| `Prefix z` | [TMUX] | Toggle pane zoom (fullscreen) |
| `Prefix q` | [TMUX] | Show pane numbers (then press number to jump) |
| `Prefix {` | [TMUX] | Swap pane with previous |
| `Prefix }` | [TMUX] | Swap pane with next |

### Tmux Copy Mode (vi-style enabled in .tmux.conf)

| Keys | Source | Action |
|------|--------|--------|
| `Prefix [` | [TMUX] | Enter copy mode |
| `q` | [TMUX] | Exit copy mode |
| `h/j/k/l` | [.tmux.conf] | Navigate in copy mode (vi mode-keys setting) |
| `v` | [.tmux.conf] | Begin selection (custom binding) |
| `y` | [.tmux.conf] | Yank selection (custom binding) |
| `/` | [TMUX] | Search forward |
| `?` | [TMUX] | Search backward |
| `n` | [TMUX] | Next search result |
| `N` | [TMUX] | Previous search result |

---

## STAGE 2: File Management (NvimTree)

### Opening/Closing NvimTree

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [NVCHAD] | Toggle NvimTree open/close |
| `Leader e` | [NVCHAD] | Focus NvimTree (cursor jumps to tree) |

**Note:** `Leader` = tap `Space` once, then release and tap the next key.

### Navigating the Tree

| Keys | Source | Action |
|------|--------|--------|
| `j` | [NVIMTREE] | Move cursor down |
| `k` | [NVIMTREE] | Move cursor up |
| `Enter` | [NVIMTREE] | Open file / expand folder |
| `o` | [NVIMTREE] | Open file / expand folder (same as Enter) |
| `l` | [NVIMTREE] | Open file / expand folder |
| `h` | [NVIMTREE] | Close folder / go to parent |
| `Tab` | [NVIMTREE] | Preview file (doesn't switch focus) |
| `Backspace` | [NVIMTREE] | Close parent folder |
| `P` | [NVIMTREE] | Go to parent directory |
| `-` | [NVIMTREE] | Navigate up one directory |
| `Ctrl+]` | [NVIMTREE] | cd into directory under cursor |

### File Operations

| Keys | Source | Action |
|------|--------|--------|
| `a` | [NVIMTREE] | Create new file (add `/` at end for folder) |
| `r` | [NVIMTREE] | Rename file/folder |
| `d` | [NVIMTREE] | Delete file/folder |
| `x` | [NVIMTREE] | Cut file/folder |
| `c` | [NVIMTREE] | Copy file/folder |
| `p` | [NVIMTREE] | Paste file/folder |
| `y` | [NVIMTREE] | Copy filename to clipboard |
| `Y` | [NVIMTREE] | Copy relative path to clipboard |
| `gy` | [NVIMTREE] | Copy absolute path to clipboard |
| `R` | [NVIMTREE] | Refresh tree |
| `H` | [NVIMTREE] | Toggle hidden files (dotfiles) |
| `I` | [NVIMTREE] | Toggle gitignored files |

### Marks and Bulk Operations

| Keys | Source | Action |
|------|--------|--------|
| `m` | [NVIMTREE] | Toggle mark on file |
| `bd` | [NVIMTREE] | Delete all marked files |
| `bmv` | [NVIMTREE] | Move all marked files |

### Split Opening

| Keys | Source | Action |
|------|--------|--------|
| `v` | [NVIMTREE] | Open in vertical split |
| `s` | [NVIMTREE] | Open in horizontal split |
| `t` | [NVIMTREE] | Open in new tab |

### Switching Focus

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [NVCHAD] | Move focus to left window |
| `Ctrl+l` | [NVCHAD] | Move focus to right window |
| `Ctrl+j` | [NVCHAD] | Move focus to window below |
| `Ctrl+k` | [NVCHAD] | Move focus to window above |

---

## STAGE 3: FZF & Shell Integration

### Shell FZF Commands (in bash/tmux pane)

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+r` | [FZF] | Fuzzy search command history |
| `Ctrl+t` | [FZF] | Fuzzy find file and insert path |
| `Alt+c` | [FZF] | Fuzzy cd into directory |

### Zoxide (Smart Directory Jump)

| Command | Source | Action |
|---------|--------|--------|
| `z <partial-name>` | [ZOXIDE] | Jump to frecent directory |
| `zi` | [ZOXIDE] | Interactive directory picker with fzf |
| `z -` | [ZOXIDE] | Jump to previous directory |

---

## STAGE 4: Buffers, Windows & Tabs

### Buffer Navigation

| Keys | Source | Action |
|------|--------|--------|
| `Tab` | [NVCHAD] | Next buffer (in tabufline) |
| `Shift+Tab` | [NVCHAD] | Previous buffer |
| `Leader x` | [NVCHAD] | Close current buffer |
| `Leader b` | [NVCHAD] | New buffer |

### Window/Split Management

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+h` | [NVCHAD] | Move to left window |
| `Ctrl+j` | [NVCHAD] | Move to window below |
| `Ctrl+k` | [NVCHAD] | Move to window above |
| `Ctrl+l` | [NVCHAD] | Move to right window |
| `:vsp` | [NEOVIM] | Create vertical split (command) |
| `:sp` | [NEOVIM] | Create horizontal split (command) |
| `Ctrl+w v` | [NEOVIM] | Create vertical split |
| `Ctrl+w s` | [NEOVIM] | Create horizontal split |
| `Ctrl+w q` | [NEOVIM] | Close current window |
| `Ctrl+w o` | [NEOVIM] | Close all windows except current |
| `Ctrl+w =` | [NEOVIM] | Make all windows equal size |
| `Ctrl+w >` | [NEOVIM] | Increase window width |
| `Ctrl+w <` | [NEOVIM] | Decrease window width |
| `Ctrl+w +` | [NEOVIM] | Increase window height |
| `Ctrl+w -` | [NEOVIM] | Decrease window height |

---

## STAGE 5: Telescope

### File Finding

| Keys | Source | Action |
|------|--------|--------|
| `Leader f f` | [NVCHAD] | Find files in project |
| `Leader f a` | [NVCHAD] | Find all files (including hidden) |
| `Leader f o` | [NVCHAD] | Find recently opened files (oldfiles) |
| `Leader f b` | [NVCHAD] | Find in open buffers |
| `Leader f z` | [NVCHAD] | Find in current buffer (fuzzy) |

### Text Search

| Keys | Source | Action |
|------|--------|--------|
| `Leader f w` | [NVCHAD] | Live grep (search text in project) |
| `Leader f W` | [NVCHAD] | Grep word under cursor |

### Telescope Pickers

| Keys | Source | Action |
|------|--------|--------|
| `Leader f h` | [NVCHAD] | Help tags |
| `Leader g t` | [NVCHAD] | Git status |
| `Leader g c` | [NVCHAD] | Git commits |
| `Leader p t` | [NVCHAD] | Pick hidden terminal |
| `Leader m a` | [NVCHAD] | Find marks |
| `Leader c m` | [NVCHAD] | Git commits |

### Theme & UI

| Keys | Source | Action |
|------|--------|--------|
| `Leader t h` | [NVCHAD] | Theme picker (preview themes!) |

### Inside Telescope Picker

| Keys | Source | Action |
|------|--------|--------|
| `Ctrl+n` | [TELESCOPE] | Move to next item |
| `Ctrl+p` | [TELESCOPE] | Move to previous item |
| `j` / `k` | [TELESCOPE] | Navigate (in normal mode) |
| `Enter` | [TELESCOPE] | Select item |
| `Ctrl+x` | [TELESCOPE] | Open in horizontal split |
| `Ctrl+v` | [TELESCOPE] | Open in vertical split |
| `Ctrl+t` | [TELESCOPE] | Open in new tab |
| `Ctrl+u` | [TELESCOPE] | Scroll preview up |
| `Ctrl+d` | [TELESCOPE] | Scroll preview down |
| `Esc` | [TELESCOPE] | Close picker (in insert mode) |
| `q` | [TELESCOPE] | Close picker (in normal mode) |

---

## STAGE 6: LSP (Language Server)

These mappings are set by NvChad's lspconfig and only activate when an LSP server attaches to a buffer.

### Code Navigation

| Keys | Source | Action |
|------|--------|--------|
| `gd` | [NVCHAD lspconfig] | Go to definition |
| `gD` | [NVCHAD lspconfig] | Go to declaration |
| `gi` | [NVCHAD lspconfig] | Go to implementation |
| `gr` | [NVCHAD lspconfig] | Go to references |
| `K` | [NVCHAD lspconfig] | Hover documentation |
| `Ctrl+k` | [NVCHAD lspconfig] | Signature help (in insert mode) |

### Code Actions

| Keys | Source | Action |
|------|--------|--------|
| `Leader c a` | [NVCHAD] | Code actions |
| `Leader r a` | [NVCHAD] | LSP rename symbol |
| `Leader f m` | [NVCHAD] | Format file |

### Diagnostics

| Keys | Source | Action |
|------|--------|--------|
| `[d` | [NVCHAD lspconfig] | Previous diagnostic |
| `]d` | [NVCHAD lspconfig] | Next diagnostic |
| `Leader d` | [NVCHAD] | Floating diagnostic |
| `Leader q` | [NVCHAD] | Diagnostic loclist |

---

## STAGE 7: Modal Editing Essentials

### Your Custom Mappings (from mappings.lua)

| Keys | Source | Action | Overrides |
|------|--------|--------|-----------|
| `;` | [mappings.lua] | Enter command mode | Overrides Neovim's `;` (repeat f/t forward) |
| `jk` | [mappings.lua] | Exit insert mode | Alternative to `Esc` |

**Note:** Because you mapped `;` to `:`, you lose the ability to repeat `f`/`t` searches forward with `;`. Use `,` for backward repeat, or press `f{char}` again.

### Modes

| Keys | Source | Action |
|------|--------|--------|
| `i` | [NEOVIM] | Insert mode (before cursor) |
| `a` | [NEOVIM] | Insert mode (after cursor) |
| `I` | [NEOVIM] | Insert at line beginning |
| `A` | [NEOVIM] | Insert at line end |
| `o` | [NEOVIM] | Insert on new line below |
| `O` | [NEOVIM] | Insert on new line above |
| `v` | [NEOVIM] | Visual mode (character) |
| `V` | [NEOVIM] | Visual line mode |
| `Ctrl+v` | [NEOVIM] | Visual block mode |
| `Esc` | [NEOVIM] | Return to normal mode |
| `jk` | [mappings.lua] | Return to normal mode (from insert) |

### Movement

| Keys | Source | Action |
|------|--------|--------|
| `h` | [NEOVIM] | Left |
| `j` | [NEOVIM] | Down |
| `k` | [NEOVIM] | Up |
| `l` | [NEOVIM] | Right |
| `w` | [NEOVIM] | Next word start |
| `W` | [NEOVIM] | Next WORD start (whitespace delimited) |
| `b` | [NEOVIM] | Previous word start |
| `B` | [NEOVIM] | Previous WORD start |
| `e` | [NEOVIM] | Next word end |
| `E` | [NEOVIM] | Next WORD end |
| `0` | [NEOVIM] | Line beginning |
| `^` | [NEOVIM] | First non-whitespace |
| `$` | [NEOVIM] | Line end |
| `gg` | [NEOVIM] | File beginning |
| `G` | [NEOVIM] | File end |
| `{` | [NEOVIM] | Previous paragraph |
| `}` | [NEOVIM] | Next paragraph |
| `%` | [NEOVIM] | Matching bracket |
| `f{char}` | [NEOVIM] | Jump to next {char} |
| `F{char}` | [NEOVIM] | Jump to previous {char} |
| `t{char}` | [NEOVIM] | Jump to before next {char} |
| `T{char}` | [NEOVIM] | Jump to after previous {char} |
| `,` | [NEOVIM] | Repeat f/t backward (`;` forward is overridden) |
| `Ctrl+d` | [NEOVIM] | Half page down |
| `Ctrl+u` | [NEOVIM] | Half page up |
| `Ctrl+f` | [NEOVIM] | Full page down |
| `Ctrl+b` | [NEOVIM] | Full page up |
| `zz` | [NEOVIM] | Center cursor line |
| `zt` | [NEOVIM] | Cursor line to top |
| `zb` | [NEOVIM] | Cursor line to bottom |

### Editing

| Keys | Source | Action |
|------|--------|--------|
| `x` | [NEOVIM] | Delete character |
| `X` | [NEOVIM] | Delete character before cursor |
| `r{char}` | [NEOVIM] | Replace character with {char} |
| `s` | [NEOVIM] | Delete character and insert |
| `S` | [NEOVIM] | Delete line and insert |
| `dd` | [NEOVIM] | Delete line |
| `D` | [NEOVIM] | Delete to line end |
| `yy` | [NEOVIM] | Yank (copy) line |
| `Y` | [NEOVIM] | Yank to line end |
| `p` | [NEOVIM] | Paste after |
| `P` | [NEOVIM] | Paste before |
| `u` | [NEOVIM] | Undo |
| `Ctrl+r` | [NEOVIM] | Redo |
| `.` | [NEOVIM] | Repeat last change |
| `~` | [NEOVIM] | Toggle case |
| `>>` | [NEOVIM] | Indent line |
| `<<` | [NEOVIM] | Unindent line |
| `J` | [NEOVIM] | Join lines |

### Operators (combine with motion)

| Keys | Source | Action |
|------|--------|--------|
| `d{motion}` | [NEOVIM] | Delete |
| `c{motion}` | [NEOVIM] | Change (delete and insert) |
| `y{motion}` | [NEOVIM] | Yank |
| `>{motion}` | [NEOVIM] | Indent |
| `<{motion}` | [NEOVIM] | Unindent |

**Examples:**
- `dw` = delete word
- `ciw` = change inner word
- `ci"` = change inside quotes
- `ct)` = change to closing paren
- `d$` = delete to end of line
- `y2j` = yank current and 2 lines below

### Text Objects

| Keys | Source | Action |
|------|--------|--------|
| `iw` | [NEOVIM] | Inner word |
| `aw` | [NEOVIM] | A word (includes space) |
| `i"` | [NEOVIM] | Inside double quotes |
| `a"` | [NEOVIM] | Around double quotes |
| `i'` | [NEOVIM] | Inside single quotes |
| `i(` or `ib` | [NEOVIM] | Inside parentheses |
| `a(` or `ab` | [NEOVIM] | Around parentheses |
| `i{` or `iB` | [NEOVIM] | Inside braces |
| `i[` | [NEOVIM] | Inside brackets |
| `it` | [NEOVIM] | Inside HTML tag |
| `ip` | [NEOVIM] | Inner paragraph |

### Search

| Keys | Source | Action |
|------|--------|--------|
| `/{pattern}` | [NEOVIM] | Search forward |
| `?{pattern}` | [NEOVIM] | Search backward |
| `n` | [NEOVIM] | Next search result |
| `N` | [NEOVIM] | Previous search result |
| `*` | [NEOVIM] | Search word under cursor forward |
| `#` | [NEOVIM] | Search word under cursor backward |
| `Leader n` | [NVCHAD] | Clear search highlight |

### Marks

| Keys | Source | Action |
|------|--------|--------|
| `m{a-z}` | [NEOVIM] | Set mark |
| `'{a-z}` | [NEOVIM] | Jump to mark line |
| `` `{a-z} `` | [NEOVIM] | Jump to mark position |
| `''` | [NEOVIM] | Jump to last position |

---

## STAGE 8: Utilities & Terminal

### Built-in Terminal

| Keys | Source | Action |
|------|--------|--------|
| `Leader h` | [NVCHAD] | Horizontal terminal |
| `Leader v` | [NVCHAD] | Vertical terminal |
| `Alt+h` | [NVCHAD] | Toggle horizontal terminal |
| `Alt+v` | [NVCHAD] | Toggle vertical terminal |
| `Alt+i` | [NVCHAD] | Toggle floating terminal |
| `Ctrl+x` | [NVCHAD] | Exit terminal mode (to normal) |

### Cheatsheet & Help

| Keys | Source | Action |
|------|--------|--------|
| `Leader c h` | [NVCHAD] | Open NvChad cheatsheet |
| `:Telescope keymaps` | [TELESCOPE] | Searchable keybindings |
| `:h {topic}` | [NEOVIM] | Neovim help |

### Line Numbers

| Keys | Source | Action |
|------|--------|--------|
| `Leader n` | [NVCHAD] | Toggle line numbers |
| `Leader r n` | [NVCHAD] | Toggle relative line numbers |

### WhichKey (built-in helper)

Press `Leader` (Space) and wait 500ms - a popup shows available keybindings. This is the [NVCHAD] which-key integration.

---

## Quick Reference: Most Important Keys

### "I need to..."

| Task | Keys | Source |
|------|------|--------|
| Open file tree | `Ctrl+n` | [NVCHAD] |
| Find file by name | `Leader f f` | [NVCHAD] |
| Search text in project | `Leader f w` | [NVCHAD] |
| Go to definition | `gd` | [NVCHAD lspconfig] |
| Switch buffer | `Tab` / `Shift+Tab` | [NVCHAD] |
| Close buffer | `Leader x` | [NVCHAD] |
| Save file | `;w Enter` | [mappings.lua] + [NEOVIM] |
| Quit | `;q Enter` | [mappings.lua] + [NEOVIM] |
| Split vertical | `Ctrl+w v` | [NEOVIM] |
| Move between windows | `Ctrl+h/j/k/l` | [NVCHAD] |
| Change theme | `Leader t h` | [NVCHAD] |
| Open cheatsheet | `Leader c h` | [NVCHAD] |

### "In tmux I need to..."

| Task | Keys | Source |
|------|------|--------|
| New pane right | `Prefix \|` | [.tmux.conf] |
| New pane below | `Prefix -` | [.tmux.conf] |
| Move between panes | `Prefix h/j/k/l` | [.tmux.conf] |
| New window | `Prefix c` | [.tmux.conf] |
| Switch window | `Prefix 1/2/3...` | [TMUX] |
| Close pane | `Prefix x` | [.tmux.conf] |
| Detach session | `Prefix d` | [TMUX] |
| Reattach session | `tmux attach -t main` | shell command |

---

## Practice Drills

### Drill 1: Tmux Navigation
1. `Prefix |` [.tmux.conf] - split right
2. `Prefix -` [.tmux.conf] - split below (now 3 panes)
3. `Prefix h` [.tmux.conf] - go left
4. `Prefix l` [.tmux.conf] - go right
5. `Prefix j` [.tmux.conf] - go down
6. `Prefix z` [TMUX] - zoom current pane
7. `Prefix z` [TMUX] - unzoom
8. `Prefix x` [.tmux.conf] - kill pane
9. `Prefix x` [.tmux.conf] - kill another

### Drill 2: File Operations
1. `Ctrl+n` [NVCHAD] - open tree
2. `j j j` [NVIMTREE] - move down
3. `a` [NVIMTREE] - create file, type `test.txt`, Enter
4. `Enter` [NVIMTREE] - open file
5. `i` [NEOVIM] - insert mode
6. Type something
7. `jk` [mappings.lua] - back to normal mode
8. `;w Enter` [mappings.lua]+[NEOVIM] - save
9. `Ctrl+n` [NVCHAD] - back to tree
10. Navigate to test.txt, press `d` [NVIMTREE] - delete
11. `y` [NVIMTREE] - confirm

### Drill 3: Telescope Workflow
1. `Leader f f` [NVCHAD] - find files
2. Type partial filename
3. `Ctrl+n` / `Ctrl+p` [TELESCOPE] - navigate results
4. `Enter` [TELESCOPE] - open
5. `Leader f w` [NVCHAD] - grep project
6. Type search term
7. `Enter` [TELESCOPE] - jump to match

### Drill 4: Buffer Workflow
1. `Leader f f` [NVCHAD] - open file 1
2. `Leader f f` [NVCHAD] - open file 2
3. `Leader f f` [NVCHAD] - open file 3
4. `Tab` [NVCHAD] - cycle forward
5. `Shift+Tab` [NVCHAD] - cycle back
6. `Leader x` [NVCHAD] - close current
7. `Tab` [NVCHAD] - to next

### Drill 5: Modal Editing
1. `gg` [NEOVIM] - go to top
2. `G` [NEOVIM] - go to bottom
3. `Ctrl+u` [NEOVIM] - half page up
4. `/function` [NEOVIM] - search for "function"
5. `n` [NEOVIM] - next match
6. `N` [NEOVIM] - previous match
7. `ciw` [NEOVIM] - change word under cursor
8. Type new word
9. `jk` [mappings.lua] - back to normal
10. `.` [NEOVIM] - repeat on another word

---

*Generated from your dotfiles configuration. When in doubt: `Leader c h` [NVCHAD]*
