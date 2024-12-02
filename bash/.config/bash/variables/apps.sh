export OPENER='xdg-open'
export GPG_TTY=$(tty)
export TERM='xterm-256color'
export COLORTERM=truecolor
export EDITOR="nvim"
export MANPAGER='less'
export PAGER='less -s -M +Gg'
export BROWSER='firefox'
export FILE='thunar'
export TERMINAL='kitty'
export LSP_USE_PLISTS=true

if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ] || [ "$TERM" = "dumb-emacs-ansi" ] && [ "$INSIDE_EMACS" ]
then
    PAGER="cat"
    alias less="cat"
    TERM=dumb-emacs-ansi
    COLORTERM=1

    EDITOR="emacsclient -a emacs -t -r"
    VISUAL="emacsclient -a emacs -t -r"
fi
