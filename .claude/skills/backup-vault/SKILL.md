---
name: backup-vault
description: Back up ~/vault/ knowledge base to git remote. Use when the user says "back up vault", "sync vault", "save vault", or after making changes to notes, specs, or org files.
---

Back up vault to git remote.

## Procedure

1. Run `cd ~/vault && git status` to see changes.

2. If no changes, report "Vault already up to date" and stop.

3. Stage all changes: `cd ~/vault && git add -A`

4. Commit with descriptive message summarizing what changed: `cd ~/vault && git commit`

5. Push to remote: `cd ~/vault && git push -u origin HEAD`

## Safety rules

- Check for untracked sensitive files before committing (API keys, credentials)
- The vault is a knowledge base — all content should be safe to push
