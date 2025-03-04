#!/bin/bash

PATH=$PATH:~/.local/scripts

dunst &

.local/scripts/dwm_status_daemon.sh &

picom -b

xrandr --output DSI-1 --rotate right

random_wallpaper.sh

exec dwm
