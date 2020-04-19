if [ "$(uname)" "==" "Darwin" ]; then
  # macOS
else
  # Assuming Linux otherwise
  export PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"
fi
export BREW_PREFIX=$(brew --prefix)

# sbin should be in path as well as there are formuale that reside there.
export PATH="$BREW_PREFIX/sbin:$PATH"

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
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
compinit
# End of lines added by compinstall
# User config

# powerline
powerline-daemon -q
POWERLINE_BASH_CONTINUATION=1
POWERLINE_BASH_SELECT=1
. $BREW_PREFIX/lib/python3.7/site-packages/powerline/bindings/zsh/powerline.zsh

# Correct locale
export LC_ALL=en_US.UTF-8

# NVM requires
export NVM_DIR="$HOME/.nvm"
[ -s "$BREW_PREFIX/opt/nvm/nvm.sh" ] && . "$BREW_PREFIX/opt/nvm/nvm.sh"  # This loads nvm
[ -s "$BREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm" ] && . "$BREW_PREFIX/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

# direnv to load environment (variables) per directory/project
eval "$(direnv hook zsh)"

# FZF config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude ".git/"'
export FZF_COMPLETION_OPTS="--preview 'bat --style=numbers --color=always {}'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {}'"
export FZF_COMPLETION_TRIGGER=',,'

# bat config
export BAT_THEME="TwoDark"

# Welcome
echo "\`odNMMmy\`                +oo:                                .hmd:"
echo "dMMNsoh:                 MMMs                                +MMMy"
echo "MMMs                     MMMs                                 .:-         \`\`             \`\`"
echo "MMMMNNNs :NNN-    NNNs   MMMMNNNm  +NNN.   \`NNN+   mNNNNNNN/ -NNN/    -smMMMMNh/     /hNMMMMms-"
echo "MMMdsss/ :MMM:    NMMs   MMMdssso  +MMM-   \`MMM+   mMMmssss- -MMM/   oMMMho+sNMMh  \`dMMmo//sMMM/"
echo "MMMs     :MMM:    NMMs   MMMs      +MMM-   \`MMM+   mMMh      -MMM/  -MMMs    ./:.  sMMM/::::hMMN"
echo "MMMs     :MMM:    NMMs   MMMs      +MMM-   \`MMM+   mMMh      -MMM/  /MMM/          hMMMNNNNNNNNN"
echo "MMMs     -MMM+   \`MMMs   MMMs      /MMM:   -MMM+   mMMh      -MMM/  -MMMy    -+/-  oMMM.    ./\`"
echo "MMMs      hMMMyosmMMN.   dMMNsohs  \`mMMNyosNMMm\`   mMMh      -MMM/   +MMMdsoyMMMy  \`hMMNyooyNMM+"
echo "mmm+       /hmMMMNdo\`    \`odNMMmh.  \`+hmMMMNho\`    dmmy      .mmm:    .odNMMNmy:     :ymNMMNds."

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/slss.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/slss.zsh
