#!/usr/bin/env bash
tmux showenv -g TMUX_GIT_BRANCH_"$(tmux display -p "#D" | tr -d %)" | sed "s/^.*=//"
