#!/usr/bin/env bash

export PATH="${PATH}:$HOME/.config/bin"

# reload .Xresources
xrdb -merge ~/.Xresources

# change keyboard layout
xmodmap ~/.Xmodmap

# enables power saving
xset dpms 240 300 360 &

# fix cursor shape
xsetroot -cursor_name left_ptr &

# make short-pressed Ctrl behave like Escape:
xcape -e 'Control_L=Escape'

# polkit
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

# java gui fixes
export _JAVA_AWT_WM_NONREPARENTING=1

# set wallpaper
feh --bg-scale $HOME/.config/wallpaper/wallpaper.jpg

# fix mouse shape
xsetroot -cursor_name left_ptr &

dunst &

#picom &

bluetoothctl power off &>/dev/null &

screensaver &
