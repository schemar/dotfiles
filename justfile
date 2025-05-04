default: help

# List available recipes
help:
  just --list --unsorted

theme-dark: theme-system-dark theme-terminal-blueberry-peach-dark
  # Slack: #24273A,#C6A0F6,#C6A0F6,#91D7E3

theme-light: theme-system-light theme-terminal-blueberry-peach-light
  # Slack: #FAF4ED,#6A67B4,#6A67B4,#007E7D

theme-system-dark:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to true'

theme-system-light:
  osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to false'

theme-terminal-macchiato:
  sd 'theme="(.*)"' 'theme="Catppuccin Macchiato"' configs/bat/default.nix
  sd 'syntax-theme = "(.*)"' 'syntax-theme = "Catppuccin Macchiato"' configs/git/default.nix
  sd 'flavour = "(.*)"' 'flavour = "macchiato"' ./configs/neovim/lua/schemar/plugins/theme.lua
  sd 'local color_scheme = "(.*)"' 'local color_scheme = "Catppuccin Macchiato"' ./configs/wezterm/wezterm.lua

  sd 'source ~/.config/tmux/(.*).conf' 'source ~/.config/tmux/catppuccin-macchiato.conf' configs/tmux/default.nix

  sd 'source ~/.config/zsh/(.*)syntax-highlighting.zsh' 'source ~/.config/zsh/catppuccin_macchiato-zsh-syntax-highlighting.zsh' configs/zsh/.zshrc
  sd 'source ~/.config/zsh/(.*)fzf-colors.zsh' 'source ~/.config/zsh/catppuccin_macchiato-fzf-colors.zsh' configs/zsh/.zshrc

  #
  # ⚠️ Remember to reload tmux and zsh sessions to apply the changes.
  #

theme-terminal-blueberry-peach-dark:
  sd 'theme="(.*)"' 'theme="blueberry_peach_dark"' configs/bat/default.nix
  sd 'syntax-theme = "(.*)"' 'syntax-theme = "blueberry_peach_dark"' configs/git/default.nix
  sd 'flavour = "(.*)"' 'flavour = "mocha"' ./configs/neovim/lua/schemar/plugins/theme.lua
  sd 'local color_scheme = "(.*)"' 'local color_scheme = "Blueberry Peach Dark"' ./configs/wezterm/wezterm.lua

  sd 'source ~/.config/tmux/(.*).conf' 'source ~/.config/tmux/blueberry_peach_dark.conf' configs/tmux/default.nix

  sd 'source ~/.config/zsh/(.*)syntax-highlighting.zsh' 'source ~/.config/zsh/blueberry_peach_dark-syntax-highlighting.zsh' configs/zsh/.zshrc
  sd 'source ~/.config/zsh/(.*)fzf-colors.zsh' 'source ~/.config/zsh/blueberry_peach_dark-fzf-colors.zsh' configs/zsh/.zshrc

  #
  # ⚠️ Remember to reload tmux and zsh sessions to apply the changes.
  #

theme-terminal-blueberry-peach-light:
  sd 'theme="(.*)"' 'theme="blueberry_peach_light"' configs/bat/default.nix
  sd 'syntax-theme = "(.*)"' 'syntax-theme = "blueberry_peach_light"' configs/git/default.nix
  sd 'flavour = "(.*)"' 'flavour = "frappe"' ./configs/neovim/lua/schemar/plugins/theme.lua
  sd 'local color_scheme = "(.*)"' 'local color_scheme = "Blueberry Peach Light"' ./configs/wezterm/wezterm.lua

  sd 'source ~/.config/tmux/blueberry(.*).conf' 'source ~/.config/tmux/blueberry_peach_light.conf' configs/tmux/default.nix

  sd 'source ~/.config/zsh/(.*)syntax-highlighting.zsh' 'source ~/.config/zsh/blueberry_peach_light-syntax-highlighting.zsh' configs/zsh/.zshrc
  sd 'source ~/.config/zsh/(.*)fzf-colors.zsh' 'source ~/.config/zsh/blueberry_peach_light-fzf-colors.zsh' configs/zsh/.zshrc

  #
  # ⚠️ Remember to reload tmux and zsh sessions to apply the changes.
  #

theme-blueberry-peach-update:
  ./tools/themes/update_blueberry_peach.lua
