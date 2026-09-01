{
  inputs,
  pkgs,
  ...
}:
let
  zsh-helix-mode = pkgs.fetchFromGitHub {
    owner = "multirious";
    repo = "zsh-helix-mode";
    rev = "e8d4940588f9809ab5658aa7c9c664921d547879";
    sha256 = "sha256-YggmEZ5hRySwXR+VZFKKZ+HzH5Cvz0661QeiAjlE30E=";
  };
in
{
  programs.zsh = {
    enable = true;
    enableCompletion = true; # Make sure this is done by home-manager, not NixOS.

    syntaxHighlighting = {
      enable = true;
    };

    shellAliases = {
      g = "git";

      l = "lsd -al";
      ll = "lsd -al --tree";

      n = "nvim";
      ng = "rm -f ~/.cache/godothost && nvim --listen ~/.cache/godothost";

      src = "source ~/.zshrc";

      fkill = "ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill";

      # Tmux usability aliases
      tma = "tmux new-session -A -s entag";
      tmd = "tmux new-session -A -s dots";
      tme = "tmux new-session -A -s eoi";
      tmh = "tmux new-session -A -s home-as";

      # Enable 256 colors in tmux
      tmux = "tmux -2";

      # Themeing for bat
      bat = "bat --theme=blueberry_peach_$(~/.config/current_theme)";

      # Themeing for lazygit
      # Use \lazygit to prevent recursive aliasing
      lazygit = "lazygit --use-config-file=\"$(\\lazygit --print-config-dir)/config.yml,$HOME/.config/lazygit/blueberry_peach_$(~/.config/current_theme).yml\"";
      lg = "lazygit";
    };

    # Have to escape zsh ${...} with ''${...}
    initContent = # sh
      ''
        #
        # ENVIRONMENT
        #

        export PATH="$HOME/.local/bin:$PATH"

        # Remove mode switching delay.
        KEYTIMEOUT=5

        # Correct locale
        export LC_ALL=en_US.UTF-8

        #
        # TOOLS
        #

        export THEME_MODE=$(~/.config/current_theme)

        #
        # COMPLETIONS
        #

        # npm run scripts
        _npm_completion() {
          local si=$IFS
          compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                       COMP_LINE=$BUFFER \
                       COMP_POINT=0 \
                       npm completion -- "''${words[@]}" \
                       2>/dev/null)
          IFS=$si
        }
        compdef _npm_completion npm

        #
        # Themeing
        #

        source ${zsh-helix-mode}/zsh-helix-mode.plugin.zsh

        function update_theme() {
          export THEME_MODE=$(~/.config/current_theme)

          if [[ "$THEME_MODE" == "light" ]]; then
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_light-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_light-fzf-colors.sh"
            ZHM_CURSOR_NORMAL="\e[0m\e[2 q\e]12;#6F58A2\a"
            ZHM_CURSOR_INSERT="\e[0m\e[2 q\e]12;#247500\a"
            ZHM_CURSOR_SELECT="\e[0m\e[2 q\e]12;#2169A6\a"
            ZHM_STYLE_CURSOR_SELECT="fg=#FAF4ED,bg=#2169A6"
            ZHM_STYLE_CURSOR_INSERT="fg=#FAF4ED,bg=#247500"
            ZHM_STYLE_OTHER_CURSOR_NORMAL="fg=#FAF4ED,bg=#A5407B"
            ZHM_STYLE_OTHER_CURSOR_SELECT="fg=#FAF4ED,bg=#017468"
            ZHM_STYLE_OTHER_CURSOR_INSERT="fg=#FAF4ED,bg=#247500"
            ZHM_STYLE_SELECTION="fg=#6B635C,bg=#F3E2D3,bold"
          else
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_dark-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_dark-fzf-colors.sh"
            ZHM_CURSOR_NORMAL="\e[0m\e[2 q\e]12;#A19DD4\a"
            ZHM_CURSOR_INSERT="\e[0m\e[2 q\e]12;#75B087\a"
            ZHM_CURSOR_SELECT="\e[0m\e[2 q\e]12;#7AA8CE\a"
            ZHM_STYLE_CURSOR_SELECT="fg=#191724,bg=#7AA8CE"
            ZHM_STYLE_CURSOR_INSERT="fg=#191724,bg=#75B087"
            ZHM_STYLE_OTHER_CURSOR_NORMAL="fg=#191724,bg=#C394C2"
            ZHM_STYLE_OTHER_CURSOR_SELECT="fg=#191724,bg=#5EB1AF"
            ZHM_STYLE_OTHER_CURSOR_INSERT="fg=#191724,bg=#75B087"
            ZHM_STYLE_SELECTION="fg=#A2A2A9,bg=#353144,bold"
          fi
        }

        update_theme
        # Auto update on signal, but only if running interactively.
        # Trigger with `pkill -USR1 zsh` or `pkill -USR1 -u "$(whoami)" zsh`
        trap '[[ $- == *i* ]] && update_theme' USR1
      '';
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  programs.lsd = {
    enable = true;

    colors = {
      user = "yellow";
      group = "yellow";
      size = {
        none = "dark_yellow";
        small = "yellow";
        medium = "yellow";
        large = "yellow";
      };
      date = {
        hour-old = "green";
        day-old = "green";
        older = "dark_green";
      };
    };

    # I manage my own aliases:
    enableZshIntegration = false;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

}
