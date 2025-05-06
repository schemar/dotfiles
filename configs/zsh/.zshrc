# Temp fix for compinit:
# Silences error on ZSH startup on macOS with multiple users.
ZSH_DISABLE_COMPFIX=true

#
# ENVIRONMENT
#

export PATH="$HOME/.local/bin:$PATH"
export VISUAL="nvim"
export EDITOR="nvim"

# Remove mode switching delay.
KEYTIMEOUT=5
# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[2 q'

  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
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

eval "$(mise activate zsh)"

export THEME_MODE=$(~/.config/current_theme)

## FZF Themeing
if [[ "$THEME_MODE" == "light" ]]; then
  source ~/.config/zsh/blueberry_peach_light-fzf-colors.zsh
else
  source ~/.config/zsh/blueberry_peach_dark-fzf-colors.zsh
fi

#
# COMPLETIONS
#

# npm run scripts
_npm_completion() {
  local si=$IFS
  compadd -- $(COMP_CWORD=$((CURRENT-1)) \
               COMP_LINE=$BUFFER \
               COMP_POINT=0 \
               npm completion -- "${words[@]}" \
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
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" gt --get-yargs-completions "${words[@]}"))
  IFS=$si
  _describe 'values' reply
}
compdef _gt_yargs_completions gt
###-end-gt-completions-###

# Themed syntax highlighting.
if [[ "$THEME_MODE" == "light" ]]; then
  source ~/.config/zsh/blueberry_peach_light-syntax-highlighting.zsh
else
  source ~/.config/zsh/blueberry_peach_dark-syntax-highlighting.zsh
fi
source /opt/homebrew/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Set git branch in tmux
# See also ~/.tmux.conf which reads the branch variable for status-right
find_git_repo () {
  local dir=.
  until [ "$dir" -ef / ]; do
    if [ -f "$dir/.git/HEAD" ]; then
      printf '%s' "$(greadlink -e $dir)/"
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
  local cwd=`greadlink -e "$(pwd)"`/
  local last_repo_len=${#TMUX_GIT_LAST_REPO}

  local repo_dir="$(find_git_repo)"

  # Could optimize here by not updating when staying in same repo:
  if [[ -z ${repo_dir} ]]; then
    # No git repo found
    tmux set-env -g TMUX_GIT_BRANCH_$pane_id  ' (not git)'
  else
    local branch='(unknown)'
    local head=$(< "${repo_dir}.git/HEAD")
    if [[ $head == ref:\ refs/heads/* ]]; then
        branch=${head#*/*/}
    elif [[ $head != '' ]]; then
        branch='(detached)'
    fi
    tmux set-env -g TMUX_GIT_BRANCH_$pane_id  " ${branch}"
  fi
}

add-zsh-hook precmd tmux_set_git
