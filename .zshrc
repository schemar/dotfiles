if [[  "$(uname)" = "Darwin" ]]; then
  # sbin should be in path as well as there are formuale that reside there.
  export PATH="$(brew --prefix)/sbin:$PATH"
else
  # Assuming Linux otherwise
  alias pbcopy='xclip -selection clipboard'
  alias pbpaste='xclip -selection clipboard -o'
fi


# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# NVM configuration (see https://github.com/lukechilds/zsh-nvm for details)
export NVM_COMPLETION=true
export NVM_LAZY_LOAD=true
export NVM_AUTO_USE=true

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(zsh-nvm fd yarn)

source $ZSH/oh-my-zsh.sh

alias g='git'
alias y='yarn'
alias l='exa -l --icons'
alias fkill='ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill'

# User configuration

# Auto-load nvm with .nvmrc due to lazy loading
# Must be after loading oh-my-zsh to find command nvm
autoload -U add-zsh-hook
load-nvmrc() {
  if [ -f "./.nvmrc" ]; then
    nvm use
  fi
}
add-zsh-hook chpwd load-nvmrc
# Call it in case the shell starts in a dir with a .nvmrc
load-nvmrc

# Aliases
# Enable 256 colors in tmux
alias tmux='tmux -2'

# History
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename "$HOME/.zshrc"

autoload -Uz compinit
# Only check for new stuff once a day to improve shell startup time
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C
# End of lines added by compinstall
# User config

# Correct locale
export LC_ALL=en_US.UTF-8

# direnv to load environment (variables) per directory/project
eval "$(direnv hook zsh)"

# staship shell prompt
export STARSHIP_CONFIG="$HOME/.config/starship.toml"
eval "$(starship init zsh)"

# FZF config
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='fd --type f --hidden --exclude ".git/"'
export FZF_COMPLETION_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers,changes --color=always {}'"
export FZF_COMPLETION_TRIGGER=',,'

# bat config
export BAT_THEME="Nord"
export BAT_STYLE="numbers,changes"

# default editor
export VISUAL=kak
export EDITOR=kak

# better man pager
export MANPAGER="bat -l man -p"

# Nord dir colors (e.g. ls or fd)
test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)

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
