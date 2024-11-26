# XDG directories
export XDG_DATA_HOME="${XDG_DATA_HOME:-"${HOME}/.local/share"}"
export XDG_CACHE_HOME="${XDG_CACHE_HOME:-"${HOME}/.cache"}"
export XDG_CONFIG_HOME="${XDG_CONFIG_HOME:-"${HOME}/.config"}"
export XDG_STATE_HOME="${XDG_STATE_HOME:-"${HOME}/.local/state"}"

if [ -f "$XDG_CONFIG_HOME/user-dirs.dirs" ]; then
	. "$XDG_CONFIG_HOME/user-dirs.dirs"
	export XDG_DESKTOP_DIR XDG_DOWNLOAD_DIR
	export XDG_DOCUMENTS_DIR XDG_MUSIC_DIR XDG_PICTURES_DIR
	export XDG_VIDEOS_DIR XDG_TEMPLATES_DIR XDG_PUBLICSHARE_DIR
    export SCREENSHOTS_DIR="$XDG_PICTURES_DIR/screenshots"
fi
