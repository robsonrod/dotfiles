# bash_profile

if [ -f "$HOME"/.config/user-dirs.dirs ]; then
	source "$HOME"/.config/user-dirs.dirs
fi

export TERM='xterm-256color'
export EDITOR='nvim'
export MANPAGER='most'
export PAGER='less'
export BROWSER='firefox'
export FILE="thunar"
export SCREENSHOTS_DIR="$XDG_PICTURES_DIR/screenshots"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc"
export SUDO_ASKPASS="$HOME/.local/bin/rofiaskpasswd"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.cargo}/bin"
export GOPATH="${XDG_DATA_HOME:-$HOME/.asdf/shims}/go"
export FZF_DEFAULT_OPTS="--layout=reverse --inline-info"

export PROJECTS="~/projects"
export PERSONAL_PROJECTS="${PROJECTS}/personal"
export WORK_PROJECTS="${PROJECTS}/work"
export SUDO_ASKPASS="${HOME}/.local/bin/rofiaskpasswd"

if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

if [ -f "$HOME/.cargo/env" ]; then
	. "$HOME/.cargo/env"
fi

if [ -f "$HOME/.local/share/env" ]; then
	. "$HOME/.local/share/bin/env"
fi
