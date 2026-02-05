{
  inputs,
  lib,
  isDarwin,
  ...
}:
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

      # Themeing for lazygit
      # Use \lazygit to prevent recursive aliasing
      lazygit = "lazygit --use-config-file=\"$(\\lazygit --print-config-dir)/config.yml,$HOME/.config/lazygit/blueberry_peach_$(~/.config/current_theme).yml\"";
      lg = "lazygit";
    };

    # Have to escape zsh ${...} with ${"$"}{...}
    initContent = # sh
      ''
        # Temp fix for compinit:
        # Silences error on ZSH startup on macOS with multiple users.
        ZSH_DISABLE_COMPFIX=true

        #
        # ENVIRONMENT
        #

        export PATH="$HOME/.local/bin:$PATH"
        export VISUAL="nvim"


        # Remove mode switching delay.
        KEYTIMEOUT=5
        # Change cursor shape for different vi modes.
        function zle-keymap-select {
          if [[ ${"$"}{KEYMAP} == vicmd ]] ||
             [[ $1 = 'block' ]]; then
            echo -ne '\e[2 q'

          elif [[ ${"$"}{KEYMAP} == main ]] ||
               [[ ${"$"}{KEYMAP} == viins ]] ||
               [[ ${"$"}{KEYMAP} = ''' ]] ||
               [[ $1 = 'beam' ]]; then
            echo -ne '\e[6 q'
          fi
        }
        zle -N zle-keymap-select
        _fix_cursor() {
           echo -ne '\e[6 q'
        }
        precmd_functions+=(_fix_cursor)

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
                       npm completion -- "${"$"}{words[@]}" \
                       2>/dev/null)
          IFS=$si
        }
        compdef _npm_completion npm

        #compdef gt
        ###-begin-gt-completions-###
        #
        # yargs command completion script
        #
        # Installation: gt completion >> ~/.zshrc
        #    or gt completion >> ~/.zprofile on OSX.
        #
        _gt_yargs_completions()
        {
          local reply
          local si=$IFS
          IFS=$'
        ' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" gt --get-yargs-completions "${"$"}{words[@]}"))
          IFS=$si
          _describe 'values' reply
        }
        compdef _gt_yargs_completions gt
        ###-end-gt-completions-###

        # Set git branch in tmux
        # See also ~/.tmux.conf which reads the branch variable for status-right
        find_git_repo () {
          local dir=.
          until [ "$dir" -ef / ]; do
            if [ -f "$dir/.git/HEAD" ]; then
              printf '%s' "$(${if isDarwin then "greadlink" else "readlink"} -e $dir)/"
            fi
            dir="../$dir"
          done

          printf '%s' ""
        }

        tmux_pane_id () {
          printf '%s' "$(tmux display -p "#D" | tr -d %)"
        }

        tmux_set_git() {
          if [[ -z  "$TMUX" ]]; then
            # Only run if in tmux
            return
          fi

          local pane_id=$(tmux_pane_id)
          local cwd=`${if isDarwin then "greadlink" else "readlink"} -e "$(pwd)"`/
          local last_repo_len=${"$"}{#TMUX_GIT_LAST_REPO}

          local repo_dir="$(find_git_repo)"

          # Could optimize here by not updating when staying in same repo:
          if [[ -z ${"$"}{repo_dir} ]]; then
            # No git repo found
            tmux set-env -g TMUX_GIT_BRANCH_$pane_id  ' (not git)'
          else
            local branch='(unknown)'
            local head=$(< "${"$"}{repo_dir}.git/HEAD")
            if [[ $head == ref:\ refs/heads/* ]]; then
                branch=${"$"}{head#*/*/}
            elif [[ $head != ''' ]]; then
                branch='(detached)'
            fi
            tmux set-env -g TMUX_GIT_BRANCH_$pane_id  " ${"$"}{branch}"
          fi
        }

        autoload -Uz add-zsh-hook
        add-zsh-hook precmd tmux_set_git

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
