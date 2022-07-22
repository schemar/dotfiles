export PATH="$HOME/.local/bin:$HOME/.yarn/bin:$PATH"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(fd yarn docker docker-compose)

source $ZSH/oh-my-zsh.sh


alias g='git'
alias y='yarn'

alias l='exa -al --git --icons'
# Use ll as interactive ls replacement
alias ll='br -sdp'

alias fkill='ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill'

# Tmux usability aliases
alias tmde='tmux new-session -A -s development'
alias tmdo='tmux new-session -A -s dotfiles'
alias tmta='tmux new-session -A -s tasks'

# User configuration

. "$HOME"/.asdf/asdf.sh
fpath=(${ASDF_DIR}/completions $fpath)

# Enable 256 colors in tmux
alias tmux='tmux -2'

# History
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep

zstyle :compinstall filename "$HOME/.zshrc"

# Allow adding ZSH hooks
autoload -U add-zsh-hook

autoload -Uz compinit
# Only check for new stuff once a day to improve shell startup time
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

# Use Vi key bindings
# The starship prompt indicates the mode:
# `>` is insert mode, `<` is for other modes
bindkey -v

# Correct locale
export LC_ALL=en_US.UTF-8

# direnv to load environment (variables) per directory/project
eval "$(direnv hook zsh)"

# z for directory jumping
eval "$(zoxide init zsh)"

# FZF config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude ".git/"'
export FZF_COMPLETION_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_COMPLETION_TRIGGER=',,'

# default editor: Emacs
# Visual: Graphical client
# Editor: Terminal client
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -t"

#
# Emacs
#

# custom functions required for vterm inside emacs
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# Do some things only if we are in a "smart" terminal
# (not Emacs TRAMP)
if [ "$TERM" != "dumb" ]; then
  # staship shell prompt
  export STARSHIP_CONFIG="$HOME/.config/starship.toml"
  eval "$(starship init zsh)"
else
  # Emacs TRAMP
  PS1=''
fi

# Hook to rename the vterm buffer
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# Sync directory between Emacs and vterm
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}
setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

# Enable Emacs commands inside the vterm shell
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

## Commands

## Run `find_file` inside vterm.
## Without argument: open dired in current directory.
## With argument: open buffer with file content.
find_file() {
    vterm_cmd find-file "$(realpath "${@:-.}")"
}
