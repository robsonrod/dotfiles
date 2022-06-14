#!/bin/sh

# Source common enviroment vars
if [ -f ~/.profile ]; then
    . ~/.profile
fi

# Disable access control for current user
xhost +SI:localuser:$USER

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# Make Java applications aware this is a non-reparenting window manager
export _JAVA_AWT_WM_NONREPARENTING=1

# Run xsettingsd to progagate font and theme settings
xsettingsd &

# Enable screen compositing
compton &

# Turn off the system bell
xset -b

# Enable screen locking on suspend
xss-lock -- slock &

# Uncomment this to start xterm instead for debugging purposes!
# Then you can manually run the window manager and log output
# > exec dbus-launch emacs -mm --debug-init --use-exwm 2>&1 | tee ~/debug.log
#xterm

teams &

# Finally launch emacs.
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
