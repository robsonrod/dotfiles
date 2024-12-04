#### General ####
set -o emacs
shopt -s histappend
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s autocd
shopt -s globstar
shopt -s cdspell
shopt -s cdable_vars

bind 'set colored-stats on'
bind 'set colored-completion-prefix on'
bind 'set echo-control-characters off'

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'TAB:menu-complete'

# If not running interactively, don't do anything
case $- in
    *i*) ;;
    *) return ;;
esac

# functions
source $HOME/.config/bash/functions/path_append
source $HOME/.config/bash/functions/path_preppend
source $HOME/.config/bash/functions/checkexec
source $HOME/.config/bash/functions/agent_ssh
source $HOME/.config/bash/functions/copy
source $HOME/.config/bash/functions/extract
source $HOME/.config/bash/functions/mkcd
source $HOME/.config/bash/functions/backupthis
source $HOME/.config/bash/functions/pyserver
source $HOME/.config/bash/functions/fzf-git.sh
source $HOME/.config/bash/functions/rgf
source $HOME/.config/bash/functions/fzf-ps

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
source $HOME/.config/bash/variables/direnv.bash
source $HOME/.config/bash/env

if [ -f /usr/share/bash-completion/bash_completion ]
then
    source /usr/share/bash-completion/bash_completion
elif [ -f /etc/bash_completion ]
then
    source /etc/bash_completion
fi

# aliases
source $HOME/.config/bash/aliases/general
source $HOME/.config/bash/aliases/colorful.sh
source $HOME/.config/bash/aliases/git
source $HOME/.config/bash/aliases/eza
source $HOME/.config/bash/aliases/zoxide

# update PS1 variable
source $HOME/.config/bash/prompt/tty.bash

path_preppend "$HOME/.config/bin"
path_preppend "$HOME/.local/bin"
path_preppend "${CARGO_HOME}/bin"
