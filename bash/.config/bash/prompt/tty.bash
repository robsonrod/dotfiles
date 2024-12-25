bash_prompt() {
    if [[ "$(tty)" == '/dev/pts/'* ]]; then
        if type starship > /dev/null 2>&1; then
            eval "$(starship init bash)"
        fi
    else
        PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\w\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
    fi
}

bash_prompt
unset -f bash_prompt
