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

source "$HOME"/.config/broot/launcher/bash/br

alias g='git'
alias y='yarn'

alias l='exa -al --git --icons'
# Use ll as interactive ls replacement
alias ll='br -sdp'
# Use br to search for a folder with broot
ld() {br --only-folders --cmd "$1;"}
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

autoload -Uz compinit
# Only check for new stuff once a day to improve shell startup time
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

# Correct locale
export LC_ALL=en_US.UTF-8

# direnv to load environment (variables) per directory/project
eval "$(direnv hook zsh)"

# staship shell prompt
export STARSHIP_CONFIG="$HOME/.config/starship.toml"
eval "$(starship init zsh)"

# z for directory jumping
eval "$(zoxide init zsh)"

# FZF config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude ".git/"'
export FZF_COMPLETION_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_COMPLETION_TRIGGER=',,'

# default editor
export VISUAL="emacsclient -c"
export EDITOR="emacsclient -c"

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
