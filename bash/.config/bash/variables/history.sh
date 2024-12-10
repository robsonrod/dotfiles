HISTCONTROL=erasedups:ignoreboth              # leading space hides commands from history
HISTFILESIZE=100000                           # increase history file size (default is 500)
HISTSIZE=${HISTFILESIZE}                       # increase history size (default is 500)
HISTTIMEFORMAT="%d/%m/%y %T "
HISTIGNORE="&:[ ]*:exit:e:clear:c:ls:ll:la:history"

if [[ $- == *i* ]]; then
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
fi
