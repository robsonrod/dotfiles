#!/usr/bin/env bash

# enables power saving
xset dpms 240 300 360 &
# polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
# set wallpaper
feh --bg-scale $HOME/.config/wallpaper/wallpaper.jpg

bluetoothctl power off &>/dev/null &

dunst &

screensaver &
