#### General ####

source ~/.env

PATH="$HOME/.local/bin":"$HOME/bin":$PATH

HISTCONTROL=ignoredups:erasedups
HISTSIZE=1000000
HISTFILESIZE=20000

PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\w\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"

shopt -s histappend
shopt -s checkwinsize
shopt -s expand_aliases

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return;;
esac

# set a fancy prompt (non-color, unless we know we "want" color)
case ${TERM} in
    xterm*|rxvt*|Eterm*|aterm|kterm|gnome*|alacritty|st-256color|konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
esac

git_branch() {
    if [ -d .git ] ; then
        printf "%s" "($(git branch 2> /dev/null | awk '/\*/{print $2}'))";
    fi
}

# starship
if [ "$OSTYPE" == "linux-gnu" ]; then
    term=$(ps -o 'cmd=' -p $(ps -o 'ppid=' -p $$));
    case $term in
        "urxvt"*|"emacs"*)
            PS1="\[$(tput bold)\]\[$(tput setaf 2)\]"'$(git_branch)'${pur}" \[$(tput sgr0)\]"$PS1
            ;;
        *)
            eval "$(starship init bash)"
            ;;
    esac
else
    eval "$(starship init bash)"
fi

# zoxide
eval "$(zoxide init bash)"

# asdf configuration
if [ -f ${HOME}/.asdf/asdf.sh ]; then
    . ${HOME}/.asdf/asdf.sh
    . ${HOME}/.asdf/completions/asdf.bash
fi


[[ -s $HOME/.aliases ]] && source ~/.aliases 


[[ -s ~/.bash_custom ]] && source ~/.bash_custom

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
        ;;
    *)
        ;;
esac

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"
[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
