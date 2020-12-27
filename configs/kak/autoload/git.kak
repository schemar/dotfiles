##
## git.kak by lenormf
##

# http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
# ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾

# Faces that highlight text that overflows the following limits:
#   - title: 50 characters
#   - body: 72 characters
set-face global GitOverflowTitle yellow
set-face global GitOverflowBody yellow

hook global WinSetOption filetype=git-(commit|rebase) %{
    add-highlighter window/ regex "^\h*[^#\s][^\n]{71}([^\n]+)" 1:GitOverflowBody
    add-highlighter window/ regex "\A[\s\n]*[^#\s][^\n]{49}([^\n]+)" 1:GitOverflowTitle
    # Auto-wrap commit message
    set window autowrap_column 71
    autowrap-enable
}
