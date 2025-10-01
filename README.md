# My Doom Emacs Config

This repository contains my personal [Doom Emacs](https://github.com/doomemacs/doomemacs) configuration.

## Installation

1. First, install Doom Emacs if you haven't already:
   ```bash
   git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
   ~/.config/emacs/bin/doom install
   ```

2. Clone this configuration repository:
   ```bash
   git clone https://github.com/ianplunkett/emacs-config ~/.config/doom
   ```

3. Sync Doom with the new configuration:
   ```bash
   ~/.config/emacs/bin/doom sync
   ```

4. Restart Emacs or reload the configuration with `M-x doom/reload` (or `SPC h r r` in evil mode).

## Files

- **init.el** - Doom module configuration (which Doom modules to enable/disable)
- **config.el** - Personal configuration (themes, fonts, custom settings)
- **packages.el** - Custom package declarations

## Customization

### Modifying Modules
Edit `init.el` to enable or disable Doom modules. After making changes, run:
```bash
~/.config/emacs/bin/doom sync
```

### Personal Settings
Edit `config.el` for personal customizations like themes, fonts, and keybindings.

### Adding Packages
Declare new packages in `packages.el` and run `doom sync` to install them.

## Resources

- [Doom Emacs Documentation](https://docs.doomemacs.org/)
- [Doom Emacs GitHub](https://github.com/doomemacs/doomemacs)
- [Doom Emacs Discord](https://doomemacs.org/discord)