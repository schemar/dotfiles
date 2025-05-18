{ ... }:
{
  programs.zsh = {
    enable = true;

    defaultKeymap = "viins";

    syntaxHighlighting = {
      enable = true;
    };

    shellAliases = {
      g = "git";
      y = "yarn";

      l = "lsd -al";
      ll = "lsd -al --tree";

      h = "function hdi(){ howdoi $* -c -n 5; }; hdi";

      n = "nvim";
      ng = "rm -f ~/.cache/godothost && nvim --listen ~/.cache/godothost";

      src = "source ~/.zshrc";

      # Required for gitui to work with git push via SSH:
      gitui = "eval $(ssh-agent) && ssh-add ~/.ssh/id_rsa && gitui";

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

        ## FZF Themeing
        if [[ "$THEME_MODE" == "light" ]]; then
          source ${./config/blueberry_peach_light-fzf-colors.zsh}
        else
          source ${./config/blueberry_peach_dark-fzf-colors.zsh}
        fi

        # Themed syntax highlighting.
        if [[ "$THEME_MODE" == "light" ]]; then
          source ${./config/blueberry_peach_light-syntax-highlighting.zsh}
        else
          source ${./config/blueberry_peach_dark-syntax-highlighting.zsh}
        fi
      '';
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
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
