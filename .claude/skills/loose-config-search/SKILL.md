---
name: loose-config-search
description: Search for untracked configuration files and unbacked-up changes. Use when the user wants to find config files not in the dotfiles MANIFEST, or check what's drifted since last backup.
---

Search for untracked and drifted configuration files.

## Procedure

### 1. Find unbacked-up changes

Read `~/dotfiles/MANIFEST`. For each tracked path, compare the live version to the dotfiles copy. Report only real drift — ignore files covered by `~/dotfiles/.gitignore` (elpa/, eln-cache/, fish_variables, etc.). If a tracked source is missing or empty in the live environment, flag it as a warning.

### 2. Check gitignore coverage

Look at what `git status` shows in `~/dotfiles/`. If there are untracked files that shouldn't be committed (generated files, caches, build artifacts), propose additions to `~/dotfiles/.gitignore`.

### 3. Check git-crypt coverage

Read `~/dotfiles/.gitattributes`. For any tracked file containing credentials, tokens, passwords, or API keys, verify it has a git-crypt filter. If a MANIFEST entry contains secrets and is NOT in .gitattributes, flag it urgently.

### 4. Find untracked config files

Search common config locations for user-edited files not covered by the MANIFEST:

- `~/.config/` — any top-level dirs or files not in MANIFEST
- `~/.*` — dotfiles in home dir not in MANIFEST
- `~/.local/share/applications/` — desktop entries
- `~/.ssh/config` (not keys)
- `~/.gitconfig`, `~/.gitignore_global`

For each untracked item found:
- Read it (briefly) to determine if it's user-edited or auto-generated
- Only report user-edited configs that look like they belong in dotfiles
- Skip noise: cache dirs, lock files, history files, auto-generated state, package manager artifacts, empty files, backup files

### 5. Report

Present:
1. **Drifted** — tracked files with real unbacked-up changes
2. **Missing source** — tracked files that no longer exist live (MANIFEST may be stale)
3. **Gitignore gaps** — files showing in git status that should be ignored
4. **Git-crypt gaps** — secret files missing encryption
5. **Untracked** — user-edited config files that might belong in the MANIFEST, with a recommendation for each

For untracked items, ask the user which (if any) to add to the MANIFEST.

## Safety rules

- NEVER read or report private keys, tokens, or credential file contents — only flag their existence
- NEVER modify the MANIFEST automatically — only propose additions
- NEVER modify .gitignore or .gitattributes automatically — only propose additions
- This is read-only — no copies, no commits
