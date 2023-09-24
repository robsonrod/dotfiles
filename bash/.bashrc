#### General ####

source ~/.env

HISTCONTROL=ignoredups:erasedups
HISTSIZE=1000000
HISTFILESIZE=20000
HISTTIMEFORMAT="%d/%m/%y %T "

PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\w\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

shopt -s histappend
shopt -s checkwinsize
shopt -s expand_aliases

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# set a fancy prompt (non-color, unless we know we "want" color)
case ${TERM} in
    xterm* | rxvt* | Eterm* | aterm | kterm | gnome* | alacritty | st-256color | konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
esac

git_branch() {
    if [ -d .git ]; then
        printf "%s" "($(git branch 2>/dev/null | awk '/\*/{print $2}'))"
    fi
}

bind 'set colored-stats on'
bind 'set colored-completion-prefix on'
bind 'set echo-control-characters off'

bind 'set show-all-if-ambiguous on'
bind 'TAB:menu-complete'

# starship
eval "$(starship init bash)"

# zoxide
eval "$(zoxide init bash)"

if [[ $- != *i* ]]; then
    return
elif [[ $TERM == urxvt* && -z "$TMUX" ]]; then
    exec tmux && exit 0
fi

# asdf configuration
if [ -f ${HOME}/.asdf/asdf.sh ]; then
    . ${HOME}/.asdf/asdf.sh
    . ${HOME}/.asdf/completions/asdf.bash
fi

[[ -s $HOME/.aliases ]] && source ~/.aliases

[[ -s $HOME/.functions ]] && source ~/.functions

[[ -s $HOME/.bash_custom ]] && source ~/.bash_custom

case "$OSTYPE" in
    "darwin"*)
        if [ -f $(brew --prefix)/etc/bash_completion ]; then
            . $(brew --prefix)/etc/bash_completion
        fi
        ;;
    "linux-gnu"*)
        if [ -f /etc/profile.d/bash_completion.sh ]; then
            . /etc/profile.d/bash_completion.sh
        fi

        if [ -f /usr/share/doc/fzf/examples/key-bindings.bash ]; then
            . /usr/share/doc/fzf/examples/key-bindings.bash
        fi
        if [ -f /usr/share/fzf/key-bindings.bash ]; then
            . /usr/share/fzf/key-bindings.bash
        fi
        ;;
    *) ;;

esac

if [ -f "$HOME/.local/share/bin/env" ]; then
    . "$HOME/.local/share/bin/env"
fi
