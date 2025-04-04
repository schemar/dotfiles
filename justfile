default: help

# List available recipes
help:
  just --list --unsorted

theme-dark: theme-system-dark theme-terminal-macchiato
  # Slack: #24273A,#C6A0F6,#C6A0F6,#91D7E3

theme-light: theme-system-light theme-terminal-latte
  # Slack: #EFF1F5,#8839EF,#8839EF,#04A5E5

theme-system-dark:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'

theme-system-light:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'

theme-terminal-macchiato:
  sd 'theme="(.*)"' 'theme="Catppuccin Macchiato"' configs/bat/config
  sd 'syntax-theme = (.*)' 'syntax-theme = Catppuccin Macchiato' configs/git/.gitconfig
  sd 'flavour = "(.*)"' 'flavour = "macchiato"' configs/neovim/lua/svim/plugins/catppuccin.lua
  sd 'color_scheme = "(.*)"' 'color_scheme = "Catppuccin Macchiato"' configs/wezterm/wezterm.lua

  # TMUX:
  sd "@catppuccin_flavor '(.*)'" "@catppuccin_flavor 'macchiato'" configs/tmux/.tmux.conf
  sd 'thm_bg="(.*)' 'thm_bg="#24273a"' configs/tmux/.tmux.conf
  sd 'thm_fg="(.*)' 'thm_fg="#cad3f5"' configs/tmux/.tmux.conf
  sd 'thm_blue="(.*)' 'thm_blue="#8aadf4"' configs/tmux/.tmux.conf

  # ZSH syntax highlighting:
  sd 'source ~/.config/zsh/catppuccin_(.*)-zsh-syntax-highlighting.zsh' 'source ~/.config/zsh/catppuccin_macchiato-zsh-syntax-highlighting.zsh' configs/zsh/.zshrc

  # FZF:
  sd 'color=bg\+.*' 'color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796 \' configs/zsh/.zshrc
  sd 'color=fg:.*' 'color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6 \' configs/zsh/.zshrc
  sd 'color=marker:.*' 'color=marker:#f4dbd6,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796"' configs/zsh/.zshrc

  #
  # ⚠️ Remember to reload tmux and zsh sessions to apply the changes.
  #

theme-terminal-latte:
  sd 'theme="(.*)"' 'theme="Catppuccin Latte"' configs/bat/config
  sd 'syntax-theme = (.*)' 'syntax-theme = Catppuccin Latte' configs/git/.gitconfig
  sd 'flavour = "(.*)"' 'flavour = "latte"' configs/neovim/lua/svim/plugins/catppuccin.lua
  sd 'color_scheme = "(.*)"' 'color_scheme = "Catppuccin Latte"' configs/wezterm/wezterm.lua

  # TMUX:
  sd "@catppuccin_flavor '(.*)'" "@catppuccin_flavor 'latte'" configs/tmux/.tmux.conf
  sd 'thm_bg="(.*)' 'thm_bg="#eff1f5"' configs/tmux/.tmux.conf
  sd 'thm_fg="(.*)' 'thm_fg="#4c4f69"' configs/tmux/.tmux.conf
  sd 'thm_blue="(.*)' 'thm_blue="#1e66f5"' configs/tmux/.tmux.conf

  # ZSH syntax highlighting:
  sd 'source ~/.config/zsh/catppuccin_(.*)-zsh-syntax-highlighting.zsh' 'source ~/.config/zsh/catppuccin_latte-zsh-syntax-highlighting.zsh' configs/zsh/.zshrc

  # FZF:
  sd 'color=bg\+.*' 'color=bg+:#ccd0da,bg:#eff1f5,spinner:#dc8a78,hl:#d20f39 \' configs/zsh/.zshrc
  sd 'color=fg:.*' 'color=fg:#4c4f69,header:#d20f39,info:#8839ef,pointer:#dc8a78 \' configs/zsh/.zshrc
  sd 'color=marker:.*' 'color=marker:#dc8a78,fg+:#4c4f69,prompt:#8839ef,hl+:#d20f39"' configs/zsh/.zshrc

  #
  # ⚠️ Remember to reload tmux and zsh sessions to apply the changes.
  #
