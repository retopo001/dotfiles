---
name: backup-both-dotfile-and-vault
description: Back up both dotfiles and vault in one command. Use when the user says "back up everything", "sync all", or wants to preserve both config and knowledge changes.
---

Back up both ~/dotfiles/ and ~/vault/ to their git remotes.

## Procedure

### Part 1: Dotfiles

1. Read `~/dotfiles/MANIFEST` to get the list of tracked paths (skip comments and blank lines).

2. For each path in the MANIFEST:
   a. **Source missing or empty**: SKIP and warn the user. NEVER copy an empty or missing source over an existing backup.
   b. **Identical**: Skip silently.
   c. **New or different**: Copy using `cp -r`.

3. Run `cd ~/dotfiles && git status` to show what changed.

4. If changes exist, commit: `cd ~/dotfiles && git add -A && git commit` with descriptive message.

5. Push: `cd ~/dotfiles && git push -u origin HEAD`

### Part 2: Vault

6. Run `cd ~/vault && git status` to see changes.

7. If changes exist, commit: `cd ~/vault && git add -A && git commit` with descriptive message.

8. Push: `cd ~/vault && git push -u origin HEAD`

### Summary

9. Report what was backed up in each repo (or "already up to date" if no changes).

## Safety rules

- NEVER overwrite a dotfile backup with empty content or a missing source
- Check for untracked sensitive files before committing
- The MANIFEST is the single source of truth for tracked dotfiles
