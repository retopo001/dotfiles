---
name: system-setup
description: Set up a fresh system from dotfiles and vault repos. Only use when explicitly asked to set up a new machine.
disable-model-invocation: true
---

Execute the LLM steps from `~/dotfiles/SETUP.md` (Part 2, Phases 1–12) in order.

## Rules

- Follow the phase ordering exactly — each phase unlocks the next
- Before each phase, verify prerequisites are met (packages installed, dirs exist)
- Use `sudo` via the sudo MCP if available, otherwise ask the user
- After each phase, briefly confirm what was done before moving to the next
- If a phase fails, stop and troubleshoot before continuing
- NEVER skip git-crypt unlock verification — encrypted files must be readable before deploying secrets
- NEVER use symlinks — copy only
