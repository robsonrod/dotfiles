export DOTFILES="${HOME}/dotfiles"
export SUDO_ASKPASS="${HOME}/.local/bin/rofiaskpasswd"
export XCURSOR_THEME="Adwaita"
export SUDO_ASKPASS="${USER_SCRIPTS_DIR}/rofiaskpasswd"
export OS_ID=$(awk -F= ' /^ID=/ { gsub("\"", ""); print $2 } ' /etc/os-release)
export COMPUTER_NAME=$(uname -a | cut -d ' ' -f2)
export NETWORK_WIRELESS_INTERFACE=$(ip -o -4 route show to default | awk '{print $5}' | grep -Ei "w" | head -1)
export NETWORK_WIRED_INTERFACE=$(ip -o -4 route show to default | awk '{print $5}' | grep -Ei "e" | head -1)
export XINITRC="${HOME}/.xinitrc"

if [ -n "$DISPLAY" ]; then
	export MONITOR=$(xrandr -q | grep " connected primary" | cut -d ' ' -f1)
fi

if [ "$COMPUTER_NAME" = "iracema" ]; then
	export GDK_SCALE=2
	export GDK_DPI_SCALE=0.5
	export QT_AUTO_SCREEN_SET_FACTOR=0
	export QT_SCALE_FACTOR=2
	export QT_FONT_DPI=96

	export FONT0="JetbrainsMono Nerd Font:style=Medium:size=18;2"
	export FONT1="JetbrainsMono Nerd Font:style=Medium:weight=Bold:size=18;2"
	export PBHEIGHT="50"
else
	export FONT0="JetbrainsMono Nerd Font:style=Medium:size=10;2"
	export FONT1="JetbrainsMono Nerd Font:style=Medium:weight=Bold:size=10;2"
	export PBHEIGHT="20"
fi
