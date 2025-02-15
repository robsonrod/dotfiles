export OPENER='xdg-open'
export GPG_TTY=$(tty)
export TERM='xterm-256color'
export COLORTERM=truecolor

if pgrep -x emacs >/dev/null; then
	export VISUAL="emacsclient -c"
	export EDITOR="emacsclient -t"
else
	export VISUAL="nvim"
	export EDITOR=$VISUAL
fi

export MANPAGER='less'
export PAGER='bat -P --style=grid --wrap=never'
export BROWSER='firefox'
export FILE='thunar'
export TERMINAL='kitty'
export LSP_USE_PLISTS=true

if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]; then
	export PAGER='bat -P --style=grid --wrap=never'
	alias less="cat"
	TERM=dumb-emacs-ansi
	COLORTERM=1

	EDITOR="emacsclient -a emacs -t -r"
	VISUAL="emacsclient -a emacs -t -r"
fi
