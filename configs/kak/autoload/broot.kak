# Integration with broot
#
# It suspends kakoune in the background and shows broot.
# When a file in broot is selected and broot is closed,
# the terminal returns to kakoune and opens the selected file.
# 
# From the wiki: https://github.com/mawww/kakoune/wiki/Integrating-Other-CLI-apps
# With improvements from https://github.com/Crote/kakoune-ranger/blob/master/ranger.kak

def toggle-broot %{
    # broot with the out switch writes the names of all selected files to the specified file when it is quit
    # for-each-line makes sure all files that ranger wrote into that tmp file are opened when back in kakoune (using edit command)
    suspend-and-resume \
        "br --conf $HOME/.config/broot/conf.hjson\;$HOME/.config/kak/autoload/broot.hjson --out /tmp/broot-files-%val{client_pid}" \
        "for-each-line edit /tmp/broot-files-%val{client_pid}"
}

