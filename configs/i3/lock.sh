#!/bin/bash

# Use feh background as lock background.
fehbg=$(grep -o "'.*'" ~/.fehbg | sed "s/'//g")

i3lock -i "$fehbg"
