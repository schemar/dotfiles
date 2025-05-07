{ ... }:
{
  programs.zsh = {
    enable = true;

    defaultKeymap = "viins";

    shellAliases = {
      g = "git";
      y = "yarn";

      l = "lsd -al";
      ll = "lsd -al --tree";

      h = "function hdi(){ howdoi $* -c -n 5; }; hdi";

      n = "nvim";
      ng = "rm -f /Users/schemar/.cache/godothost && nvim --listen /Users/schemar/.cache/godothost";

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

    initExtra = ''
      ${builtins.readFile ./.zshrc}
    '';
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
  };

}
