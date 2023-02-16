# bash_profile

if [ -f "$HOME/.config/user-dirs.dirs" ]; then
	. "$HOME/.config/user-dirs.dirs"
fi

export TERM='screen-256color'
export EDITOR='nvim'
export MANPAGER='less'
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

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
fi

if [ -f "$HOME/.cargo/env" ]; then
	. "$HOME/.cargo/env"
fi

if [ -f "$HOME/.local/share/env" ]; then
	. "$HOME/.local/share/bin/env"
fi

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
. "$HOME/.cargo/env"
