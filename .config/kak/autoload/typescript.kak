define-command -hidden eslint-format %{
    evaluate-commands -draft -no-hooks -save-regs '|' %{
        cursor_store_position

        # Select all to format
        execute-keys '%'

        # eslint does a fix-dry-run with a json formatter which results in a JSON output to stdout that includes the fixed file.
        # jq then extracts the fixed file output from the JSON. -j returns the raw output without any escaping.
        set-register '|' %{
            %sh{
                echo "$kak_selection" | \
                npx eslint --format json \
                           --fix-dry-run \
                           --stdin \
                           --stdin-filename "$kak_buffile" | \
                jq -j ".[].output"
            }
        }

        # Replace all with content from register:
        execute-keys '|<ret>'

        cursor_restore_position
    }
}

define-command -hidden prettier-format %{
    evaluate-commands -draft -no-hooks -save-regs '|' %{
        cursor_store_position

        # Select all to format
        execute-keys '%'

        # Run prettier on the selection from stdin
        set-register '|' %{
            %sh{
                echo "$kak_selection" | \
                npx prettier --stdin-filepath $kak_buffile
            }
        }

        # Replace all with content from register:
        execute-keys '|<ret>'

        cursor_restore_position
    }
}

define-command -hidden tslint-format %{
    set-option window autoreload 'yes'
    write
    nop %sh{
        npx tslint --fix "$kak_buffile"
    }
    hook -once window BufReload .* %{
        unset-option window autoreload
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
