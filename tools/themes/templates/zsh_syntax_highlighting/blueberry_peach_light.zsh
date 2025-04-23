# Catppuccin Latte Theme (for zsh-syntax-highlighting)
#
# Paste this files contents inside your ~/.zshrc before you activate zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main cursor)
typeset -gA ZSH_HIGHLIGHT_STYLES

# Main highlighter styling: https://github.com/zsh-users/zsh-syntax-highlighting/blob/master/docs/highlighters/main.md
#
## General
### Diffs
### Markup
## Classes
## Comments
ZSH_HIGHLIGHT_STYLES[comment]='fg={{surface2}}'
## Constants
## Entitites
## Functions/methods
ZSH_HIGHLIGHT_STYLES[alias]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[global-alias]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[function]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[command]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[precommand]='fg={{green}},italic'
ZSH_HIGHLIGHT_STYLES[autodirectory]='fg={{peach}},italic'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg={{peach}}'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg={{peach}}'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg={{mauve}}'
## Keywords
## Built ins
ZSH_HIGHLIGHT_STYLES[builtin]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg={{green}}'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg={{green}}'
## Punctuation
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg={{red}}'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-unquoted]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[process-substitution-delimiter]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-delimiter]='fg={{red}}'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg={{red}}'
ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]='fg={{red}}'
## Serializable / Configuration Languages
## Storage
## Strings
ZSH_HIGHLIGHT_STYLES[command-substitution-quoted]='fg={{yellow}}'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-quoted]='fg={{yellow}}'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg={{yellow}}'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument-unclosed]='fg={{maroon}}'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg={{yellow}}'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument-unclosed]='fg={{maroon}}'
ZSH_HIGHLIGHT_STYLES[rc-quote]='fg={{yellow}}'
## Variables
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg={{maroon}}'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[assign]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[named-fd]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg={{text}}'
## No category relevant in spec
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg={{maroon}}'
ZSH_HIGHLIGHT_STYLES[path]='fg={{text}},underline'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg={{red}},underline'
ZSH_HIGHLIGHT_STYLES[path_prefix]='fg={{text}},underline'
ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg={{red}},underline'
ZSH_HIGHLIGHT_STYLES[globbing]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg={{mauve}}'
#ZSH_HIGHLIGHT_STYLES[command-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[command-substitution-unquoted]='fg=?'
#ZSH_HIGHLIGHT_STYLES[process-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[arithmetic-expansion]='fg=?'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg={{maroon}}'
ZSH_HIGHLIGHT_STYLES[redirection]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[arg0]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[default]='fg={{text}}'
ZSH_HIGHLIGHT_STYLES[cursor]='fg={{text}}'
