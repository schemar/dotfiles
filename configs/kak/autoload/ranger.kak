# Integration with ranger
#
# It suspends kakoune in the background and shows ranger.
# When a file in ranger is selected and ranger is closed,
# the terminal returns to kakoune and opens the selected file.
# 
# From the wiki: https://github.com/mawww/kakoune/wiki/Integrating-Other-CLI-apps
# With improvements from https://github.com/Crote/kakoune-ranger/blob/master/ranger.kak

def toggle-ranger %{
    # ranger with the choosefiles switch writes the names of all selected files to the specified file when it is quit
    # for-each-line makes sure all files that ranger wrote into that tmp file are opened when back in kakoune (using edit command)
    suspend-and-resume \
        "ranger --choosefiles=/tmp/ranger-files-%val{client_pid}" \
        "for-each-line edit /tmp/ranger-files-%val{client_pid}"
}

