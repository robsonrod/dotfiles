set -gx XDG_DATA_HOME "$HOME/.local/share"
set -gx XDG_CACHE_HOME "$HOME/.cache"
set -gx XDG_CONFIG_HOME "$HOME/.config"
set -gx XDG_STATE_HOME "$HOME/.local/state"

if test -f "$XDG_CONFIG_HOME/user-dirs.dirs"
	fenv source "$XDG_CONFIG_HOME/user-dirs.dirs"
    set -gx SCREENSHOTS_DIR "$XDG_PICTURES_DIR/screenshots"
end
