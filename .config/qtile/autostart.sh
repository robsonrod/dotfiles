#!/bin/sh

/usr/bin/dunst &
#/usr/libexec/xfce-polkit &
feh --bg-fill $HOME/.config/wallpaper/wallpaper.jpg
udiskie -T &
thunar --daemon &
nm-applet &

$HOME/.config/picom/launch.sh &

xsetroot -cursor_name left_ptr &
