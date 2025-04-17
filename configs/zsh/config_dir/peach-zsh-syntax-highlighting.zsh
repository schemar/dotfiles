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
ZSH_HIGHLIGHT_STYLES[comment]='fg=#e0d6c8'
## Constants
## Entitites
## Functions/methods
ZSH_HIGHLIGHT_STYLES[alias]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[global-alias]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[function]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[command]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=#3a8a53,italic'
ZSH_HIGHLIGHT_STYLES[autodirectory]='fg=#996329,italic'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=#6b6aa0'
## Keywords
## Built ins
ZSH_HIGHLIGHT_STYLES[builtin]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=#3a8a53'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=#3a8a53'
## Punctuation
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=#b74e5f'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-unquoted]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[process-substitution-delimiter]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-delimiter]='fg=#b74e5f'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=#b74e5f'
ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]='fg=#b74e5f'
## Serializable / Configuration Languages
## Storage
## Strings
ZSH_HIGHLIGHT_STYLES[command-substitution-quoted]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-quoted]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument-unclosed]='fg=#97597b'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=#996329'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument-unclosed]='fg=#97597b'
ZSH_HIGHLIGHT_STYLES[rc-quote]='fg=#996329'
## Variables
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg=#97597b'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[assign]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[named-fd]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg=#45405e'
## No category relevant in spec
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#97597b'
ZSH_HIGHLIGHT_STYLES[path]='fg=#45405e,underline'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=#b74e5f,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=#45405e,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=#b74e5f,underline'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=#6b6aa0'
#ZSH_HIGHLIGHT_STYLES[command-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[command-substitution-unquoted]='fg=?'
#ZSH_HIGHLIGHT_STYLES[process-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[arithmetic-expansion]='fg=?'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg=#97597b'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[arg0]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[default]='fg=#45405e'
ZSH_HIGHLIGHT_STYLES[cursor]='fg=#45405e'
