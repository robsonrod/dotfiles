HISTCONTROL=erasedups:ignoreboth              # leading space hides commands from history
HISTFILESIZE=100000                           # increase history file size (default is 500)
HISTSIZE=${HISTFILESIZE}                       # increase history size (default is 500)
HISTTIMEFORMAT="%d/%m/%y %T "
HISTIGNORE="&:[ ]*:exit:e:clear:c:ls:ll:la:history"

alias hh=hstr                    # hh to be alias for hstr
shopt -s histappend              # append new history items to .bash_history
# ensure synchronization between bash memory and history file
export PROMPT_COMMAND="history -a; history -n; ${PROMPT_COMMAND}"
function hstrnotiocsti {
    { READLINE_LINE="$( { </dev/tty hstr ${READLINE_LINE}; } 2>&1 1>&3 3>&- )"; } 3>&1;
    READLINE_POINT=${#READLINE_LINE}
}
# if this is interactive shell, then bind hstr to Ctrl-r (for Vi mode check doc)
if [[ $- =~ .*i.* ]]; then bind -x '"\C-r": "hstrnotiocsti"'; fi
export HSTR_TIOCSTI=n

if [[ $- == *i* ]]; then
    bind '"\e[A": history-search-backward'
    bind '"\e[B": history-search-forward'
fi
