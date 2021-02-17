if [[  "$(uname)" = "Darwin" ]]; then
  # sbin should be in path as well as there are formuale that reside there.
  export PATH="$(brew --prefix)/sbin:$PATH"
else
  # Assuming Linux otherwise
  #alias pbcopy='wl-copy --trim-newline'
  #alias pbpaste='wl-paste --no-newline'
  # Using xclip instead of wayland version
  alias pbcopy='xclip -selection clipboard'
  alias pbpaste='xclip -selection clipboard -o'
fi
  
export PATH="$HOME/.local/bin:$PATH"

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
plugins=(zsh-nvm fd yarn docker docker-compose)

source $ZSH/oh-my-zsh.sh
source "$HOME"/.config/broot/launcher/bash/br

alias g='git'
alias y='yarn'
alias l='ls -al'
# Use bb as a one-shot ls replacement
# Reduce height to not overflow
# Use -H to hide hidden files
bb() {br -sdp --height=$(echo "$(tput lines) - 1" | bc) --cmd ":pt" "$@"}
# Use b as interactive ls replacement
alias b='br -sdp'
# Use bd to search for a folder with broot
bd() {br --only-folders --cmd "$1;"}
alias fkill='ps -efl | fzf | awk '\''{print $4}'\'' | xargs kill'

# Tmux usability aliases
alias tmde='tmux new-session -A -s development'
alias tmdo='tmux new-session -A -s dotfiles'
alias tmta='tmux new-session -A -s tasks'

# Kakoune session management
# Use k to launch kakoune with "managed" session names.
k() {
  if [ -n "$TMUX" ]; then
    # When in tmux, use the tmux session and window as session name.
    # We have to replace $ and @ to end up with a valis session name for kakoune.
    name=$(tmux display-message -p "#{session_id}-#{window_id}" | sed 's/\$\([0-9]*\)-@\([0-9]*\)/tmux-s\1-w\2/')
  else
    # Otherwise use the same session for kakoune clients
    # that are launched from the same directory.
    name=$(pwd | sed 's/\//_/g')
  fi

  kak -clear;
  if kak -l | grep -q -x "$name"; then
    kak -c "$name" "$@"
  else
    kak -s "$name" "$@"
  fi
}

# User configuration

. "$HOME"/.asdf/asdf.sh
fpath=(${ASDF_DIR}/completions $fpath)

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

# eval navi (terminal cheat sheets; activate with C-g)
eval "$(navi widget zsh)"

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
export VISUAL=kak
export EDITOR=kak

# better man pager
export MANPAGER="bat -l man -p"

# Nord dir colors (e.g. ls or fd)
test -r "~/.dir_colors" && eval $(dircolors ~/.dir_colors)

# Welcome
printf "\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n%s\n" \
"  \`odNMMmy\`                +oo:                                .hmd:" \
"  dMMNsoh:                 MMMs                                +MMMy" \
"  MMMs                     MMMs                                 .:-         \`\`             \`\`" \
"  MMMMNNNs :NNN-    NNNs   MMMMNNNm  +NNN.   \`NNN+   mNNNNNNN/ -NNN/    -smMMMMNh/     /hNMMMMms-" \
"  MMMdsss/ :MMM:    NMMs   MMMdssso  +MMM-   \`MMM+   mMMmssss- -MMM/   oMMMho+sNMMh  \`dMMmo//sMMM/" \
"  MMMs     :MMM:    NMMs   MMMs      +MMM-   \`MMM+   mMMh      -MMM/  -MMMs    ./:.  sMMM/::::hMMN" \
"  MMMs     :MMM:    NMMs   MMMs      +MMM-   \`MMM+   mMMh      -MMM/  /MMM/          hMMMNNNNNNNNN" \
"  MMMs     -MMM+   \`MMMs   MMMs      /MMM:   -MMM+   mMMh      -MMM/  -MMMy    -+/-  oMMM.    ./\`" \
"  MMMs      hMMMyosmMMN.   dMMNsohs  \`mMMNyosNMMm\`   mMMh      -MMM/   +MMMdsoyMMMy  \`hMMNyooyNMM+" \
"  mmm+       /hmMMMNdo\`    \`odNMMmh.  \`+hmMMMNho\`    dmmy      .mmm:    .odNMMNmy:     :ymNMMNds." \
| lolcat

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/sls.zsh
# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[[ -f /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/slss.zsh ]] && . /Users/masc/Projects/vwfs/ERNST/ernst-vehicle-import-trigger/node_modules/tabtab/.completions/slss.zsh
