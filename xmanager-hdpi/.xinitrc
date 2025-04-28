#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager here)

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# rice

xset +fp /usr/share/fonts/local &
xset fp rehash &
xrdb -merge $HOME/.Xresources &
xmodmap ~/.Xmodmap &

setxkbmap -option ctrl:nocaps
setxkbmap br

# wm
exec bspwm
