{ inputs, pkgs, ... }:
{
  programs.tmux = {
    enable = true;

    shell = "${pkgs.zsh}/bin/zsh";
    terminal = "xterm-256color";

    focusEvents = true;

    escapeTime = 0;
    historyLimit = 10000;

    prefix = "C-a";
    keyMode = "vi";
    mouse = true;

    baseIndex = 1;
    clock24 = true;
    customPaneNavigationAndResize = true;

    # Disable due to messed up shell.
    # See, among others, https://discourse.nixos.org/t/tmux-use-bash-instead-defined-zsh-in-home-manager/54763/2
    sensibleOnTop = false;

    # Fetch later versions, as some packaged plugins are outdated:
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "catppuccin";
          name = "tmux-plugin-catppuccin";
          rtpFilePath = "catppuccin.tmux";
          src = fetchFromGitHub {
            owner = "catppuccin";
            repo = "tmux";
            rev = "14a546fb64dc1141e5d02bac2185d8c1fd530d6a";
            sha256 = "sha256-poG3QCow2j6h/G7BLEA8v3ZJXuk28iPmH1J4t7vT55k=";
          };
        };
        extraConfig = # tmux
          ''
            source ${./config/reset_catppuccin.conf}
            if-shell '[ "$(~/.config/current_theme)" = "dark" ]' \
              "source-file ${inputs.blueberry-peach}/ports/tmux/blueberry_peach_dark.conf" \
              "source-file ${inputs.blueberry-peach}/ports/tmux/blueberry_peach_light.conf"

            set -g status-left '#[bg=#{?client_prefix,blue,default},fg=#{?client_prefix,black,default}]#S#[default] '

            # Make sure to do this before sourcing tmux-continuum.
            # See their "known-issues".
            # git branch; see file in dotfiles repo (tmux/config)
            set -g status-right '#(${./config/pane_branch.sh})'
            # The branch name is often longer than the default length of 40.
            set -g status-right-length 70
          '';
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
          rtpFilePath = "extrakto.tmux";
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
          rtpFilePath = "yank.tmux";
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
          rtpFilePath = "vim-tmux-navigator.tmux";
          src = fetchFromGitHub {
            owner = "christoomey";
            repo = "vim-tmux-navigator";
            rev = "33afa80db65113561dc53fa732b7f5e53d5ecfd0";
            sha256 = "sha256-h3c5ki8N4kiNzbgjxHwLh625un6GqbLZv/4dPVW3vCI=";
          };
        };
        extraConfig = # tmux
          ''
            set -g @vim_navigator_mapping_left "M-h"
            set -g @vim_navigator_mapping_right "M-l"
            set -g @vim_navigator_mapping_up "M-k"
            set -g @vim_navigator_mapping_down "M-j"
            set -g @vim_navigator_mapping_prev "M-\\"
          '';
      }
      tmuxPlugins.resurrect
      # Do tmux-continuum last.
      # 1. It requires tmux-resurrect.
      # 2. It must be the last to set right-status (see their "known-issues").
      {
        plugin = tmuxPlugins.mkTmuxPlugin {
          pluginName = "continuum";
          name = "tmux-plugin-continuum";
          rtpFilePath = "continuum.tmux";
          src = fetchFromGitHub {
            owner = "tmux-plugins";
            repo = "tmux-continuum";
            rev = "0698e8f4b17d6454c71bf5212895ec055c578da0";
            sha256 = "sha256-W71QyLwC/MXz3bcLR2aJeWcoXFI/A3itjpcWKAdVFJY=";
          };
        };
        extraConfig = # tmux
          ''
            set -g @continuum-restore 'on'
            set -g @continuum-boot 'on'
            set -g @continuum-save-interval '10' # minutes
          '';
      }
    ];

    extraConfig = # tmux
      ''
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
}
