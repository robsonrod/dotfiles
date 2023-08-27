#!/usr/bin/env bash

/usr/libexec/at-spi-bus-launcher --laucher-immediately &
xdg-user-dirs-gtk-update &
/usr/bin/dunst &
/usr/lib/policykit-1-gnome/polkit-gnome-authentication-agent-1 &
feh --bg-scale $HOME/.config/qtile/wallpaper/wallpaper.jpg
udiskie -T &
thunar --daemon &
nm-applet &
xautolock -time 3 -locker screen_lock &
/usr/bin/cshell/launcher &
/usr/bin/teams %U &
psensor &
start-pulseaudio-x11 &
/usr/bin/snap userd --autostart &
xiccd &
/usr/bin/nvidia-settings --load-config-only &
blueman-applet &
compton -b &
xsetroot -cursor_name left_ptr &
