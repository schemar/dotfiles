hook global WinSetOption filetype=(javascript|typescript) %{
    # Prettier
    define-command prettier-format %{
        execute-keys '%|npx prettier --stdin-filepath $kak_buffile<ret><space>;'
    }
    map window development p -docstring 'format with npx prettier' ': prettier-format<ret>'

    # Eslint
    set window lintcmd 'run() { cat "$1" | npx eslint -f $BREW_PREFIX/lib/node_modules/eslint-formatter-kakoune/index.js --stdin --stdin-filename $kak_buffile;} && run '
    map window development l -docstring 'lint with npx eslint' ': lint<ret>'
    lint-enable

    define-command eslint-format %{
        execute-keys ': w<ret>: evaluate-commands %sh{npx eslint --fix -f BREW_PREFIX/lib/node_modules/eslint-formatter-kakoune/index.js $kak_buffile}<ret>'
    }
    map window development f -docstring 'format with npx eslint' ': eslint-format<ret>'

    # You can add these to a `.kakrc.local` in your project:
    # To auto-prettier on write:
    # hook buffer BufWritePre .* prettier-format
    # To auto-eslint-fix on write:
    # hook buffer BufWritePost .* eslint-format
    # To auto-eslint on write:
    # hook buffer BufWritePost .* lint
}
