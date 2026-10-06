Personal Arch Linux dotfiles for Japhir. No build system — configs are deployed by symlinking files to their proper destinations under `~/.config/`, `~/bin/`, etc.

## Current environment stack

- **Window manager**: Niri + DMS (Dank Material Shell) — provides integrated workspaces/bar, no separate bar process
- **Editor**: Emacs with evil-mode — central to the entire workflow
- **Terminal**: Ghostty + Zsh (with VI mode, fzf, zoxide, starship)
- **Email**: mu4e inside Emacs, synced via mbsync
- **Notes**: org-roam (zettelkasten) + org-mode GTD
- **File manager**: yazi
- **Music**: ncspot (Spotify TUI)

Legacy configs retained but not active: Hyprland, Sway, Waybar, AShell, Polybar, i3, eww, jay, scroll. `gitui/` is an experiment, not in active use.

## Branches

- `master` — the laptop (current active branch; intended to be renamed to `main`).
- `desktop` — old gaming desktop, dead since ~2024. Stale, kept for reference only.
- `macos` — mac-only patches for a work laptop running macOS (AeroSpace, LaunchAgents, pi sandbox, …). Pushed to `origin/macos`; kept rebased on top of `master`. Generic changes (agents, emacs, bin) go to `master` first.

## Emacs configuration

The Emacs config source of truth is `myinit.org` (org-babel literate config at repo root). On startup `init.el` calls `org-babel-load-file` on it, which tangles + loads it. **Always edit `myinit.org` — never `myinit.el`.** `myinit.el` is generated output, gitignored, and regenerated on every startup.

The GTD system is configured in `myinit.org`; its docs are `GLOSSARY.org` (language), `gtd-workflow.org` (as built) and `docs/adr/` (decisions).

Machine-specific instructions live in `CLAUDE.local.md` (tracked, different content on `master` and `macos`; auto-loaded by Claude Code). Same for the user-level `agents/CLAUDE.local.md`, imported from `agents/CLAUDE.md`.

The files under `emacs/` (`early-init.el`, `custom-modules/`) were a temporary experiment to integrate `crafted-emacs`. They are not the canonical source.

`emacs-starter/` is a separate, standalone Emacs config for the shared org todo system (target: a Windows machine; phones run orgzly-revived and beorg). It shares nothing with `myinit.org`; its literate file is `config.org`. Setup steps and design notes are in its `README.org`.

## Systemd user services

In `systemd/user/`, symlinked to `~/.config/systemd/user/`:

| Unit | Purpose |
|---|---|
| `emacs.service` | Emacs daemon |
| `mbsync@.service` / `.timer` | Email sync |
| `orggcalsync.service` / `.timer` | Google Calendar → org sync |
| `clipcat.service` | Clipboard manager |

## Personal scripts (`bin/`)

Executable scripts deployed to `~/bin/`. Notable ones:

- `fuzzy_update.sh` — interactive pacman update with fzf
- `handle_monitor_connect.sh` — hotplug display management
- `block_distractions` / `unblock_distractions` — host-file based site blocking
- `emacs-popup` — launch floating Emacs frame
- `checkbatnotify.sh` — battery notification daemon

## Config format notes

- **Niri** config (`niri/config.kdl` + `niri/dms/`) uses KDL format; DMS config lives in `niri/dms/`
- **Rofi**, **Mako**, **Kanshi**, **Foot** use their own INI/plain-text formats
