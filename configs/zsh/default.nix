{
  config,
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
    completionInit = # zsh
      ''
        # Tell Zsh that compinit is an autoloadable native Zsh function.
        # -U prevents alias expansion while loading it.
        autoload -Uz compinit

        # Evaluated by Nix at Home Manager build time.
        # Produces a value such as abc123-home-manager-path.
        _hm_generation=${baseNameOf "${config.home.path}"}

        # ''${ escapes Nix interpolation, producing a literal for Zsh to evaluate later.
        #
        # Resulting cache name contains:
        # - the Zsh version
        # - the current Home Manager environment identifier
        _zdump=''${XDG_CACHE_HOME:-$HOME/.cache}/zsh/zcompdump-$ZSH_VERSION-$_hm_generation

        # ''${_zdump:h} is Zsh syntax for the dump file's parent directory.
        # Create that directory if it does not yet exist.
        [[ -d ''${_zdump:h} ]] || mkdir -p ''${_zdump:h}

        # Initialize completion and use this file for compinit's textual cache.
        compinit -d "$_zdump"

        # Compile when the bytecode cache is absent, empty, or older than
        # the textual completion dump.
        if [[ ! -s "$_zdump.zwc" || "$_zdump" -nt "$_zdump.zwc" ]]; then
          zcompile -R -- "$_zdump"
        fi

        # Do not leave helper parameters in the interactive shell.
        unset _zdump _hm_generationation
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
    initContent = # zsh
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
        # Trigger with: pkill -USR1 -u "$UID" -x zsh
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
