# AI Agent Context: EFS Emacs Configuration

This document serves as a context file for AI agents to understand the structure, philosophy, and state of this Emacs configuration.

## 1. Project Overview
**Name**: EFS (Emacs Configuration)
**Location**: `~/.dotfiles/.emacs.d/efs/` (Symlinked to `~/.emacs.d/efs/`)
**Entry Point**: `init.el`
**Launch Script**: `efs` (usually in `~/.local/bin/`)

## 2. Architecture & Philosophy
- **Modular**: Configuration is split into functional modules located in the `lisp/` directory.
- **Evil-centric**: VIM keybindings are the first-class citizens (`init-evil.el`).
- **Leader Key**: All custom bindings use `general.el` on the `SPC` prefix (`init-keys.el`).
- **Performance**: Lazy loading via `use-package`, tuned GC in `init.el`.
- **Modern UI**: Uses `vertico`, `corfu`, `doom-themes`, `doom-modeline`, and `nerd-icons`.

## 3. Module Structure (`lisp/`)
| File | Purpose | Key Packages |
| :--- | :--- | :--- |
| `init.el` | Bootloader | `gc-cons-threshold` |
| `init-pkg.el` | Package Management | `melpa`, `use-package` |
| `init-base.el` | Defaults | `utf-8`, `no-backup`, `electric-pair` |
| `init-ui.el` | Visuals | `doom-themes`, `nerd-icons` |
| `init-evil.el` | Vim Emulation | `evil`, `evil-collection` |
| `init-keys.el` | Key definitions | `general` |
| `init-tools.el` | Utilities | `vertico`, `corfu`, `magit`, `yasnippet`, `restart-emacs` |
| `init-prog.el` | Coding | `eglot`, `treesit-auto`, `apheleia` |
| `init-org.el` | Org Mode | `org-modern`, `org-indent` |
| `init-files.el` | File Explorer | `dirvish` (configured as sidebar) |
| `init-containers.el`| Container Dev | `docker.el`, `tramp` (configured for Podman) |

## 4. Keybinding Strategy
- **Prefix**: `SPC`
- **Logic**: Mnemonic (e.g., `f` for file, `b` for buffer, `w` for window, `c` for code).
- **Avoid**: `C-x` patterns are remapped to Leader keys where possible.
- **Critical Keys**:
    - `SPC e`: File Explorer (Dirvish Sidebar)
    - `SPC .` / `SPC f f`: Find File
    - `SPC c f`: Format Buffer
    - `SPC o a`: Org Agenda
    - `SPC d`: Podman/Docker Menu
    - `SPC w`: Window Management (`w/`, `w-`, `wd`, `wm`, `ww`, `w=`)
    - `SPC q`: Quit/Restart

## 5. Development Environment
- **LSP**: Built-in `Eglot` is used.
- **Treesitter**: `treesit-auto` handles grammar installation and mode mapping automatically.
- **Formatting**: `apheleia` handles auto-formatting on save/demand.
- **Containers**: `init-containers.el` forces `docker.el` to use the `podman` binary. TRAMP is configured for `/podman:container:/path` access.

## 6. Maintenance Notes
- **Symlinks**: The actual config resides in `.dotfiles`. Ensure `~/.emacs.d/efs` symlinks to it correctly.
- **Startup**: Run `efs` to start. Use `efs --debug-init` to troubleshoot.
- **Testing**: Can use `emacs --init-directory ~/.emacs.d/efs --batch` to verify load capability.

## 7. Recent Modifications (Log)
- **Dirvish**: Configured as left-sidebar (`treemacs`-like).
- **Vertico**: Enabled directory navigation with Backspace.
- **Podman**: Fixed binary path to `podman`.
- **Keys**: Refactored window and system keys to Leader.
- **Org**: Added `init-org.el` with `org-modern`.
- **Treesitter**: Switched to `treesit-auto` to fix installation errors.
