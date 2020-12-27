# This should work with simple marks 'Z' 'z', but somehow the command
# execute-keys 'Z'
# doesn't store a mark in the register ^ and then the command
# execute-keys 'z'
# complains that there is nothing in the ^ register

declare-option -hidden int cursor_return_line 0
declare-option -hidden int cursor_return_column 0

define-command -hidden cursor_store_position %{
    set-option window cursor_return_line %val{cursor_line}
    set-option window cursor_return_column %val{cursor_column}
}

define-command -hidden cursor_restore_position %{
    execute-keys "<space>;gl%opt{cursor_return_line}g%opt{cursor_return_column}l"
}
