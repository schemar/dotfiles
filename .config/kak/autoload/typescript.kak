define-command -hidden eslint-format %{
    execute-keys ': w<ret>'
    nop %sh{
        npx eslint --fix "$kak_buffile"
    }
}

define-command -hidden prettier-format %{
    execute-keys '%|npx prettier --stdin-filepath $kak_buffile<ret><space>;'
}

define-command -hidden tslint-format %{
    execute-keys ': w<ret>'
    nop %sh{
        npx tslint --fix "$kak_buffile"
    }
}

# Setting eslint as linter and default formatter
hook global WinSetOption filetype=(javascript|typescript) %{
    # Eslint
    set window lintcmd 'run() { cat "$1" | npx eslint -f $BREW_PREFIX/lib/node_modules/eslint-formatter-kakoune/index.js --stdin --stdin-filename $kak_buffile;} && run '
    map window development l -docstring 'lint' ': lint<ret>'
    lint-enable

    # Formatting
    # Overwrite this alias in a `.kakrc.local` to use one of the other formatters
    # that are defined commands above.
    alias window tsformat eslint-format
    map window development f -docstring 'format' ': tsformat<ret>'
}
