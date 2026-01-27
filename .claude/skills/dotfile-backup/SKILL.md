---
name: dotfile-backup
description: Back up tracked dotfiles from live environment to ~/dotfiles/. Use when the user says "back up dotfiles", "sync dotfiles", "save my config", or after making config changes they want to preserve.
---

Back up dotfiles from live environment to ~/dotfiles/ using the MANIFEST.

## Procedure

1. Read `~/dotfiles/MANIFEST` to get the list of tracked paths (skip comments and blank lines).

2. For each path in the MANIFEST:
   a. **Source missing or empty**: SKIP and warn the user. NEVER copy an empty or missing source over an existing backup. This is the most important safety rule.
   b. **Destination missing in dotfiles**: Show the user what's new and ask to confirm before copying.
   c. **Identical**: Skip silently.
   d. **Different**: Show a diff summary (files changed, key differences) and ask the user to confirm before overwriting.

3. After all entries are reviewed and confirmed, copy approved changes using `cp -r`.

4. Run `cd ~/dotfiles && git status` to show what changed.

5. Ask the user if they want to commit. If yes, `cd ~/dotfiles && git add -A && git commit` with a descriptive message.

## Safety rules

- NEVER overwrite a backup with empty content or a missing source
- NEVER copy without showing diffs first when content differs
- NEVER auto-commit — always ask
- Each changed path gets its own confirmation
- The MANIFEST is the single source of truth for what is tracked
