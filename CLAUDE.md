# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration for macOS. Doom Emacs is a configuration framework for Emacs that pre-configures packages and provides a modular system.

## Common Commands

After modifying configuration files, run:
```bash
~/.config/emacs/bin/doom sync
```

To reload Emacs configuration without restarting:
- `M-x doom/reload` or `SPC h r r`

## File Structure

- `init.el` - Controls which Doom modules are enabled (completion, UI, tools, languages)
- `config.el` - Personal configuration and customizations
- `packages.el` - Additional packages beyond Doom's defaults
- `custom.el` - Emacs-generated custom settings (org-agenda-files, faces)
- `modules/gtd-roam.el` - Custom GTD (Getting Things Done) workflow integrated with org-roam

## Key Configuration Areas

### Language Support
Enabled languages with LSP: JavaScript/TypeScript, Python (pyright), JSON, YAML, Web, Docker, Terraform. Tree-sitter enabled for JS/TS, Python, JSON, YAML, Web.

### Org-mode/GTD System
The `modules/gtd-roam.el` module implements a GTD workflow with:
- Johnny.Decimal category structure for organization
- Custom TODO states: TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELLED
- Urgency/Impact priority system (1-4 scale) that auto-calculates A/B/C priorities
- Capture templates for todos, notes, journal entries, and projects
- Files stored in `~/org/` with GTD files in `~/org/10-19_productivity/11_gtd/`
- org-roam notes in `~/org/20-29_knowledge_management/21_org_roam/`

### Key Bindings (Leader = SPC)
- `SPC o c` - Clean agenda (10-day view)
- `SPC o C` - Complete view (agenda + unscheduled TODOs)
- `SPC o D` - Daily agenda
- `SPC o W` - Weekly agenda
- `SPC o P` - Priority matrix view
- `SPC o w` - Weekly review (split view)
- `SPC P a/n/s` - Project views (all/with next actions/stalled)
- `SPC n f` - Find org-roam node
- `SPC X` - Capture

In org-mode (`SPC m p`):
- `u` - Set urgency
- `i` - Set impact
- `p` - Auto-set priority from urgency/impact

### Email (mu4e)
Configured for Gmail via OfflineIMAP with GTD-style folders (@Action, @Waiting For, @Reference, @Reading List).

### Additional Packages
- `org-roam-ui` - Visual graph interface for org-roam
- `prettier-js` - Auto-formatting for TypeScript/JS
- `gptel` - LLM integration
- `claude-code-ide` - Claude Code Emacs integration (bound to `C-c C-'`)
