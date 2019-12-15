if [ "$(uname)" "==" "Darwin" ]; then
  # macOS
  export PIP_LIB="/usr/local"
else
  # Assuming Linux otherwise
  export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
  export PIP_LIB="/home/linuxbrew/.linuxbrew"
fi

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git yarn brew fd)

source $ZSH/oh-my-zsh.sh

# User configuration

# Aliases
# Enable 256 colors in tmux
alias tmux='tmux -2'

# History
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/Users/masc/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# User config

# powerline
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $PIP_LIB/lib/python3.7/site-packages/powerline/bindings/zsh/powerline.zsh

# Correct locale
export LC_ALL=en_US.UTF-8

# NVM requires
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && . "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# direnv to load environment (variables) per directory/project
eval "$(direnv hook zsh)"

# FZF config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden'
export FZF_COMPLETION_OPTS="--preview 'bat --style=numbers --color=always {}'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {}'"
export FZF_COMPLETION_TRIGGER=',,'

# bat config
export BAT_THEME="TwoDark"
