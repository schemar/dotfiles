export PATH="$HOME/.local/bin:$HOME/.yarn/bin:$PATH"

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(asdf fd npm yarn)

source $ZSH/oh-my-zsh.sh


alias g='git'
alias y='yarn'

alias l='lsd -al'
alias ll='lsd -al --tree'

alias h='function hdi(){ howdoi $* -c -n 5; }; hdi'

alias fkill='ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill'

# Tmux usability aliases
alias tmde='tmux new-session -A -s development'
alias tmdo='tmux new-session -A -s dotfiles'
alias tmta='tmux new-session -A -s tasks'

# User configuration

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

export VISUAL="nvim"
export EDITOR="nvim"

# staship shell prompt
export STARSHIP_CONFIG="$HOME/.config/starship.toml"
eval "$(starship init zsh)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/schemar/Projects/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/schemar/Projects/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/schemar/Projects/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/schemar/Projects/google-cloud-sdk/completion.zsh.inc'; fi

#compdef gt
###-begin-gt-completions-###
#
# yargs command completion script
#
# Installation: /opt/homebrew/bin/gt completion >> ~/.zshrc
#    or /opt/homebrew/bin/gt completion >> ~/.zprofile on OSX.
#
_gt_yargs_completions()
{
  local reply
  local si=$IFS
  IFS=$'
' reply=($(COMP_CWORD="$((CURRENT-1))" COMP_LINE="$BUFFER" COMP_POINT="$CURSOR" /opt/homebrew/bin/gt --get-yargs-completions "${words[@]}"))
  IFS=$si
  _describe 'values' reply
}
compdef _gt_yargs_completions gt
###-end-gt-completions-###

