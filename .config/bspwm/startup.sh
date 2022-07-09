#!/bin/sh

/usr/bin/dunst &
/usr/libexec/xfce-polkit &
feh --bg-fill $HOME/.config/wallpaper/solar-system.jpg
udiskie -s &
thunar --daemon &

$HOME/.config/sxhkd/launch.sh &
$HOME/.config/picom/launch.sh &
$HOME/.config/polybar/launch.sh &
$HOME/.config/bspwm/scripts/low_bat_notifier.sh

xsetroot -cursor_name left_ptr &
