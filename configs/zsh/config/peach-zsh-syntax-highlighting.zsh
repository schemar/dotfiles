# Blueberry Peach Light Theme (for zsh-syntax-highlighting)
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
ZSH_HIGHLIGHT_STYLES[comment]='fg=#9C8282'
## Constants
## Entitites
## Functions/methods
ZSH_HIGHLIGHT_STYLES[alias]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[global-alias]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[function]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[command]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=#288043,italic'
ZSH_HIGHLIGHT_STYLES[autodirectory]='fg=#AC591C,italic'
ZSH_HIGHLIGHT_STYLES[single-hyphen-option]='fg=#AC591C'
ZSH_HIGHLIGHT_STYLES[double-hyphen-option]='fg=#AC591C'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument]='fg=#6A67B4'
## Keywords
## Built ins
ZSH_HIGHLIGHT_STYLES[builtin]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[reserved-word]='fg=#288043'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=#288043'
## Punctuation
ZSH_HIGHLIGHT_STYLES[commandseparator]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-unquoted]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[process-substitution-delimiter]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-delimiter]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[back-double-quoted-argument]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[back-dollar-quoted-argument]='fg=#C53F64'
## Serializable / Configuration Languages
## Storage
## Strings
ZSH_HIGHLIGHT_STYLES[command-substitution-quoted]='fg=#8A7400'
ZSH_HIGHLIGHT_STYLES[command-substitution-delimiter-quoted]='fg=#8A7400'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument]='fg=#8A7400'
ZSH_HIGHLIGHT_STYLES[single-quoted-argument-unclosed]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument]='fg=#8A7400'
ZSH_HIGHLIGHT_STYLES[double-quoted-argument-unclosed]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[rc-quote]='fg=#8A7400'
## Variables
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[dollar-quoted-argument-unclosed]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[dollar-double-quoted-argument]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[assign]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[named-fd]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[numeric-fd]='fg=#5B5B5B'
## No category relevant in spec
ZSH_HIGHLIGHT_STYLES[unknown-token]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[path]='fg=#5B5B5B,underline'
ZSH_HIGHLIGHT_STYLES[path_pathseparator]='fg=#C53F64,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix]='fg=#5B5B5B,underline'
ZSH_HIGHLIGHT_STYLES[path_prefix_pathseparator]='fg=#C53F64,underline'
ZSH_HIGHLIGHT_STYLES[globbing]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[history-expansion]='fg=#6A67B4'
#ZSH_HIGHLIGHT_STYLES[command-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[command-substitution-unquoted]='fg=?'
#ZSH_HIGHLIGHT_STYLES[process-substitution]='fg=?'
#ZSH_HIGHLIGHT_STYLES[arithmetic-expansion]='fg=?'
ZSH_HIGHLIGHT_STYLES[back-quoted-argument-unclosed]='fg=#C53F64'
ZSH_HIGHLIGHT_STYLES[redirection]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[arg0]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[default]='fg=#5B5B5B'
ZSH_HIGHLIGHT_STYLES[cursor]='fg=#5B5B5B'
