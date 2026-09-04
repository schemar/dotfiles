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
  mkInit = name: cmd: pkgs.runCommand "${name}-init.zsh" { } "${cmd} > $out";
  zoxideInit = mkInit "zoxide" "${pkgs.zoxide}/bin/zoxide init zsh";
  direnvInit = mkInit "direnv" "${pkgs.direnv}/bin/direnv hook zsh";
in
{
  programs.zsh = {
    enable = true;
    # Make sure this is done by home-manager, not NixOS:
    # (see also programs.zsh.enableCompletion = false in systems/common.nix)
    enableCompletion = true;
    completionInit = ''
      autoload -Uz compinit
      _zdump=''${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump-$ZSH_VERSION
      [[ -d ''${_zdump:h} ]] || mkdir -p ''${_zdump:h}
      _zfresh=( $_zdump(N.mh-24) )
      if (( $#_zfresh )); then
        compinit -C -d "$_zdump"
      else
        compinit -d "$_zdump"
        { zcompile -R -- "$_zdump" } &!
      fi
      unset _zdump _zfresh
    '';

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
      bat = "bat --theme=blueberry_peach_\${THEME_MODE:-dark}";

      lg = "lazygit";
    };

    # Have to escape zsh ${...} with ''${...}
    initContent = # sh
      ''
        source ${zoxideInit}
        source ${direnvInit}

        #
        # ENVIRONMENT
        #

        export PATH="$HOME/.local/bin:$PATH"

        # Remove mode switching delay.
        KEYTIMEOUT=5

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
          # Reads faster than $(~./...):
          export THEME_MODE=''${$(<~/.config/current_theme_store):-dark}

          # Themeing for lazygit. Do it here so we don't have to do it on every startup.
          local lg_base="$(lazygit --print-config-dir)"
          export LG_CONFIG_FILE="$lg_base/config.yml,$HOME/.config/lazygit/blueberry_peach_''${THEME_MODE}.yml"

          if [[ "$THEME_MODE" == "light" ]]; then
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_light-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_light-fzf-colors.sh"
            source "${inputs.blueberry-peach}/ports/zsh_helix_mode/blueberry_peach_light-helix-mode.sh"
          else
            source "${inputs.blueberry-peach}/ports/zsh_syntax_highlighting/blueberry_peach_dark-syntax-highlighting.sh"
            source "${inputs.blueberry-peach}/ports/fzf/blueberry_peach_dark-fzf-colors.sh"
            source "${inputs.blueberry-peach}/ports/zsh_helix_mode/blueberry_peach_dark-helix-mode.sh"
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
    enableZshIntegration = false;
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
    enableZshIntegration = false;
  };

}
