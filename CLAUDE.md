# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a Doom Emacs configuration for macOS. Doom Emacs is a configuration framework for Emacs that pre-configures packages and provides a modular system.

## Common Commands

After modifying `init.el` or `packages.el`, run:
```bash
~/.config/emacs/bin/doom sync
```

After modifying `config.el`, `modules/gtd-roam.el`, or `modules/gtd-daily-review.el`, reload without restarting:
- `M-x doom/reload` or `SPC h r r`

To install TypeScript/TSX tree-sitter grammars (one-time setup):
- `M-x treesit-install-language-grammar` → select `typescript` or `tsx`

## File Structure

- `init.el` - Controls which Doom modules are enabled (completion, UI, tools, languages)
- `config.el` - Personal configuration and customizations
- `packages.el` - Additional packages beyond Doom's defaults
- `custom.el` - Emacs-generated custom settings (org-agenda-files, faces)
- `modules/gtd-roam.el` - Custom GTD (Getting Things Done) workflow integrated with org-roam
- `modules/gtd-daily-review.el` - Daily customer review workflow (cycles through active customers)

## Key Configuration Areas

### Language Support
Enabled languages with LSP: JavaScript/TypeScript, Python (pyright), JSON, YAML, Web, Docker, Terraform. Tree-sitter enabled for JS/TS, Python, JSON, YAML, Web.

### Daily Customer Review (`SPC o d`)
The `modules/gtd-daily-review.el` module implements a repeatable daily review workflow:
1. Reads active customers (TODO/NEXT) from `customers.org`
2. For each customer, opens a capture under their `daily` heading — type a note and `C-c C-c` to save, or `C-c C-k` to skip the customer entirely
3. If saved, opens a second capture under their `inbox` heading for an optional TODO — `C-c C-c` to save or `C-c C-k` to skip
4. After all customers, opens the Complete View agenda (`C`)

### Org-mode/GTD System
The `modules/gtd-roam.el` module implements a GTD workflow with:
- Johnny.Decimal category structure for organization
- Custom TODO states: TODO, NEXT, WAITING, SOMEDAY, DONE, CANCELLED
- Urgency/Impact priority system (1-4 scale) that auto-calculates A/B/C priorities
- Files stored in `~/org/` with GTD files in `~/org/10-19_productivity/11_gtd/`
- org-roam notes in `~/org/20-29_knowledge_management/21_org_roam/`

**Capture templates** (`SPC X`):
- `t` - Todo with priority assessment (prompts for urgency, impact, deadline → inbox.org)
- `q` - Quick todo (no priority prompts → inbox.org)
- `n` - Note → inbox.org
- `j` - Journal entry (datetree → `~/org/50-59_resources/51_journal/journal.org`)
- `p` - Project (simple template → `gtd/projects.org`)
- `o` - Centralized project (full template → `30-39_projects/30_active_projects/projects.org`)

**org-agenda-files** are managed in `custom.el` (Emacs-generated). To add a file to the agenda: `M-x org-agenda-file-to-front`. Currently includes: inbox.org, all customer org-roam notes, journal.org, todo.org.

### Key Bindings (Leader = SPC)
- `SPC o d` - Daily customer review (cycles through all active customers)
- `SPC o c` - Clean agenda (10-day view)
- `SPC o C` - Complete view (agenda + unscheduled TODOs)
- `SPC o D` - Daily agenda
- `SPC o W` - Weekly agenda
- `SPC o N` - Next actions
- `SPC o P` - Priority matrix view
- `SPC o w` - Weekly review (splits: Complete View + Priority Matrix side by side)
- `SPC P a/n/s` - Project views (all/with next actions/stalled)
- `SPC n f` - Find org-roam node
- `SPC n i` - Insert org-roam node link
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
