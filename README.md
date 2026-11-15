# dotfiles

My dotfiles, managed with [yadm](https://yadm.io/).

## Setup

1. Install yadm (e.g. `brew install yadm` on macOS).
2. Clone and deploy:

   ```
   yadm clone https://github.com/marcy/dotfiles.git
   ```

3. Install the tools listed under Dependencies as needed.

## Contents

- Shells: zsh (`.zshrc`, zsh-abbr in `.config/zsh-abbr/`), fish (`.config/fish/`, Oh My Fish in `.config/omf/`)
- Editors: Emacs (`.emacs.d/init.el`), Vim (`.vimrc`)
- Git: `.gitconfig_global`, `.config/git/ignore` (pager: delta; ghq root: `~/src`)
- Terminal: Ghostty (`.config/ghostty/`), tmux (`.tmux.conf`)
- macOS: Hammerspoon (`.hammerspoon/init.lua`)
- Misc: peco, tig, htop, colordiff, Ruby (`.gemrc`, `.irbrc`), MySQL client prompt (`.my.cnf`)

## Dependencies

Tools referenced by these configs include: zsh, fish + Oh My Fish, zsh-abbr, peco, delta, ghq, tig, tmux, Ghostty, Hammerspoon, Emacs.

## Notes

- Some settings assume macOS paths (e.g. the Emacs path in `.gitconfig_global`). For other OSes, yadm alternate files (the `##OS.<os>` suffix) can hold per-OS variants.
