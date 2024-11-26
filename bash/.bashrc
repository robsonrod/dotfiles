#### General ####
if [[ $- == *i* ]]; then
    source ~/.local/share/blesh/ble.sh --noattach
fi

set -o emacs
shopt -s histappend
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s autocd   # Para auto-cd: cambiar de directorio sin tener que usar cd
shopt -s globstar # Glob '**' empareja recursivamente debajo
shopt -s cdspell  # cd a directorio aun si deletraste mal 1 letra
shopt -s cdable_vars  # cd a nombres de variables, sin tener que poner '$'

# update PS1 variable
source $HOME/.config/bash/prompt/tty.bash

# dont duplicate env
source $HOME/.config/bash/functions/path_append
source $HOME/.config/bash/functions/path_preppend

# used to ckeck app
source $HOME/.config/bash/functions/checkexec

# variables
source $HOME/.config/bash/variables/xdg.sh
source $HOME/.config/bash/variables/xdgfix.sh
source $HOME/.config/bash/variables/apps.sh
source $HOME/.config/bash/variables/configs.sh
source $HOME/.config/bash/variables/eza.sh
source $HOME/.config/bash/variables/fzf.sh
source $HOME/.config/bash/variables/history.sh
source $HOME/.config/bash/variables/man.sh
source $HOME/.config/bash/variables/asdf.bash

source $HOME/.config/bash/env

# functions
source $HOME/.config/bash/functions/agent_ssh
source $HOME/.config/bash/functions/copy
source $HOME/.config/bash/functions/extract
source $HOME/.config/bash/functions/mkcd

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# set a fancy prompt (non-color, unless we know we "want" color)
case ${TERM} in
    xterm* | rxvt* | Eterm* | aterm | kterm | gnome* | alacritty | kitty | st-256color | konsole*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\007"'
        ;;
    screen*)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/\~}\033\\"'
        ;;
esac

# zoxide
if checkexec zoxide; then
    eval "$(zoxide init bash)"
fi

if [[ $- != *i* ]]; then
    return
elif [[ $TERM == urxvt* && -z "$TMUX" ]]; then
    exec tmux && exit 0
fi

# aliases
source $HOME/.config/bash/aliases/general
source $HOME/.config/bash/aliases/git
source $HOME/.config/bash/aliases/colorful.sh

[ -s $HOME/.config/bash/bash_custom ] && source $HOME/.config/bash/bash_custom

path_preppend "$HOME/.local/bin"
path_preppend "${CARGO_HOME}/bin"

[[ ! ${BLE_VERSION-} ]] || ble-attach
