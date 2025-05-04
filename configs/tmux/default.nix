{ pkgs, ... }: {
  programs.tmux = {
    enable = true;

    baseIndex = 1;
    clock24 = true;
    customPaneNavigationAndResize = true;
    escapeTime = 0;

    historyLimit = 10000;
    keyMode = "vi";
    prefix = "C-a";

    mouse = true;

    shell = "${pkgs.zsh}/bin/zsh";
    terminal = "xterm-256color";

    tmuxp.enable = true;
    # Disable due to messed up shell.
    # See, among others, https://discourse.nixos.org/t/tmux-use-bash-instead-defined-zsh-in-home-manager/54763/2
    sensibleOnTop = false;
    # Fetch later versions, as some packaged plugins are very outdated:
    # TODO: Add all rtpFilePaths:
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "catppuccin";
          name = "tmux-plugin-catppuccin";
          src = fetchFromGitHub {
            owner = "catppuccin";
            repo = "tmux";
            rev = "14a546fb64dc1141e5d02bac2185d8c1fd530d6a";
            sha256 = "sha256-poG3QCow2j6h/G7BLEA8v3ZJXuk28iPmH1J4t7vT55k=";
          };
        };
        extraConfig = ''
          source ~/.config/tmux/blueberry_peach_light.conf
        '';
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "tmux-prefix-highlight";
          name = "tmux-plugin-tmux-prefix-highlight";
          rtpFilePath = "prefix_highlight.tmux";
          src = fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-prefix-highlight";
            rev = "06cbb4ecd3a0a918ce355c70dc56d79debd455c7";
            sha256 = "sha256-wkMm2Myxau24E0fbXINPuL2dc8E4ZYe5Pa6A0fWhiw4=";
          };
        };
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "fzf-url";
          name = "tmux-plugin-fzf-url";
          rtpFilePath = "fzf-url.tmux";
          src = fetchFromGitHub {
            owner = "wfxr";
            repo = "tmux-fzf-url";
            rev = "16381dce1c30fedd75fc775f887be7ea6c7cbf55";
            sha256 = "sha256-n3dEUnu0jd1MiWKRCr3HpWlq6Lw4eCYOKLbG30QgSx0=";
          };
        };
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "extrakto";
          name = "tmux-plugin-extrakto";
          src = fetchFromGitHub {
            owner = "laktak";
            repo = "extrakto";
            rev = "3eb7eb498a493e06edbe898bc70c7b87bf1a630e";
            sha256 = "sha256-pzinDE6zRne470Hid5Y53e5ZmUjieCsD/6xghBO3898=";
          };
        };
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "yank";
          name = "tmux-plugin-yank";
          src = fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-yank";
            rev = "acfd36e4fcba99f8310a7dfb432111c242fe7392";
            sha256 = "sha256-/5HPaoOx2U2d8lZZJo5dKmemu6hKgHJYq23hxkddXpA=";
          };
        };
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "vim-tmux-navigator";
          name = "tmux-plugin-vim-tmux-navigator";
          src = fetchFromGitHub {
            owner = "christoomey";
            repo = "vim-tmux-navigator";
            rev = "33afa80db65113561dc53fa732b7f5e53d5ecfd0";
            sha256 = "sha256-h3c5ki8N4kiNzbgjxHwLh625un6GqbLZv/4dPVW3vCI=";
          };
        };
        extraConfig = ''
          # Smart pane switching with awareness of Vim splits.
          # See: https://github.com/christoomey/vim-tmux-navigator
          is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
              | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?\.?(view|l?n?vim?x?|fzf)(diff)?(-wrapped)?$'"
          bind-key -n 'M-h' if-shell "$is_vim" 'send-keys M-h'  'select-pane -L'
          bind-key -n 'M-j' if-shell "$is_vim" 'send-keys M-j'  'select-pane -D'
          bind-key -n 'M-k' if-shell "$is_vim" 'send-keys M-k'  'select-pane -U'
          bind-key -n 'M-l' if-shell "$is_vim" 'send-keys M-l'  'select-pane -R'
          bind-key -n 'M-\' if-shell \"$is_vim\" 'send-keys M-\\'  'select-pane -l'
          
          bind-key -T copy-mode-vi 'M-h' select-pane -L
          bind-key -T copy-mode-vi 'M-j' select-pane -D
          bind-key -T copy-mode-vi 'M-k' select-pane -U
          bind-key -T copy-mode-vi 'M-l' select-pane -R
          bind-key -T copy-mode-vi 'M-\' select-pane -l
        '';
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "resurrect";
          name = "tmux-plugin-resurrect";
          src = fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-resurrect";
            rev = "cff343cf9e81983d3da0c8562b01616f12e8d548";
            sha256 = "sha256-FcSjYyWjXM1B+WmiK2bqUNJYtH7sJBUsY2IjSur5TjY=";
          };
        };
      }
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "continuum";
          name = "tmux-plugin-continuum";
          src = fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-continuum";
            rev = "0698e8f4b17d6454c71bf5212895ec055c578da0";
            sha256 = "sha256-W71QyLwC/MXz3bcLR2aJeWcoXFI/A3itjpcWKAdVFJY=";
          };
        };
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-boot 'on'
          set -g @continuum-save-interval '10' # minutes
        '';
      }
    ];

    extraConfig = ''
      # git branch; see file in dotfiles repo (tmux/config)
      set -g status-right '#(~/.config/tmux/pane_branch.sh)'

      # Colors
      # https://gist.github.com/andersevenrud/015e61af2fd264371032763d4ed965b6
      set -sg terminal-overrides ",*:RGB"

      # use <prefix> v/s for splitting
      bind v split-window -v
      bind s split-window -h

      # use <prefix> z to (un)zoom current pane
      bind z resize-pane -Z
      # use C-b to go to last pane and zoom
      # binding with -n means "no prefix"
      bind -n C-b select-pane -l \; resize-pane -Z

      # Move between windows
      bind -n M-J next-window
      bind -n M-K previous-window
      bind -n M-L last-window

      # resize panes more easiyly
      bind < resize-pane -L 10
      bind > resize-pane -R 10
      bind - resize-pane -D 4
      bind + resize-pane -U 4

      set -g status-interval 1 # Refresh status line every second
    '';
  };

  xdg.configFile."tmux/pane_branch.sh".source = ./config/pane_branch.sh;
  xdg.configFile."tmux/blueberry_peach_light.conf".source = ./config/blueberry_peach_light.conf;
  xdg.configFile."tmux/blueberry_peach_dark.conf".source = ./config/blueberry_peach_dark.conf;
}

