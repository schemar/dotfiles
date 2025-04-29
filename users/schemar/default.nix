{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "schemar";
  home.homeDirectory = /Users/schemar;

  home.packages = with pkgs; [ ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

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
    };

    initExtra = ''
      ${builtins.readFile ../../configs/zsh/.zshrc}
    '';
  };
}
