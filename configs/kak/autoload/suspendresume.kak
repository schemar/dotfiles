# It suspends kakoune in the background and shows a command.
# When the command ends, the terminal returns to kakoune.
# If a second parameter was given, that kak command will run after kakoune
# resumed.
# 
# From the wiki: https://github.com/mawww/kakoune/wiki/Integrating-Other-CLI-apps
# With improvements from https://github.com/Crote/kakoune-ranger/blob/master/ranger.kak

def suspend-and-resume \
    -params 1..2 \
    -docstring 'suspend-and-resume <cli command> [<kak command after resume>]: backgrounds current kakoune client and runs specified cli command.  Upon exit of command the optional kak command is executed.' \
    %{ evaluate-commands %sh{

    post_resume_cmd="$2"

   if [ -z ${TMUX_PANE+x} ]; then
       echo "fail \"Cannot open command: this only works inside tmux.\""
       exit
   fi

    # Schedule the command to run after kak is sent to background (see sleep)
    # Auto "clear" and "fg" when commans is done
    nohup sh -c "sleep 0.1; tmux send-keys -t $TMUX_PANE -l \"$1; clear; fg\"; tmux send-keys -t $TMUX_PANE \"Enter\"" > /dev/null 2>&1 &
    # Send kakoune client to the background
    /bin/kill -SIGTSTP $kak_client_pid

    # ...At this point the kakoune client is paused until the "; fg" gets run in the nohup cmd

    # Upon resume, run the kak command if is specified
    if [ ! -z "$post_resume_cmd" ]; then
        echo "$post_resume_cmd"
    fi
}}

def for-each-line \
    -docstring "for-each-line <command> <path to file>: run command with the value of each line in the file" \
    -params 2 \
    %{ evaluate-commands %sh{

    while read f; do
        printf "$1 $f\n"
    done < "$2"
}}
