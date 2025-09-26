{ inputs, ... }:
{
  programs.zsh = {
    enable = true;

    defaultKeymap = "viins";

    syntaxHighlighting = {
      enable = true;
    };

    shellAliases = {
      g = "git";
      gtsync = "gt co main && gt sync && git switch -";
      gtsubmit = "gt top && gt sync && gt submit && git switch -";

      y = "yarn";

      l = "lsd -al";
      ll = "lsd -al --tree";

      h = "function hdi(){ howdoi $* -c -n 5; }; hdi";

      n = "nvim";
      ng = "rm -f ~/.cache/godothost && nvim --listen ~/.cache/godothost";

      src = "source ~/.zshrc";

      fkill = "ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill";

      # Tmux usability aliases
      tma = "tmux new-session -A -s afilio";
      tmd = "tmux new-session -A -s dots";
      tme = "tmux new-session -A -s eoi";
      tmh = "tmux new-session -A -s home-as";

      # Enable 256 colors in tmux
      tmux = "tmux -2";

      # Themeing for bat
      bat = "bat --theme=blueberry_peach_$(~/.config/current_theme)";
    };

    initContent = # sh
      ''
        ${builtins.readFile ./.zshrc}


        function update_theme() {
          export THEME_MODE=$(~/.config/current_theme)

          if [[ "$THEME_MODE" == "light" ]]; then
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_light-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_light-fzf-colors.sh"
          else
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_dark-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_dark-fzf-colors.sh"
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
