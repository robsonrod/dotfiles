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

BASHDIR="${HOME}/.config/bash"

# functions
source ${BASHDIR}/functions/path_append
source ${BASHDIR}/functions/path_preppend
source ${BASHDIR}/functions/checkexec
source ${BASHDIR}/functions/agent_ssh
source ${BASHDIR}/functions/copy
source ${BASHDIR}/functions/extract
source ${BASHDIR}/functions/mkcd
source ${BASHDIR}/functions/backupthis
source ${BASHDIR}/functions/pyserver
source ${BASHDIR}/functions/fzf-git.sh
source ${BASHDIR}/functions/rgf
source ${BASHDIR}/functions/fzf-ps
source ${BASHDIR}/functions/cdd
source ${BASHDIR}/functions/truecolor


# update PS1 variable
source ${BASHDIR}/prompt/tty.bash

# variables
source ${BASHDIR}/variables/xdg.sh
source ${BASHDIR}/variables/xdgfix.sh
source ${BASHDIR}/variables/apps.sh
source ${BASHDIR}/variables/configs.sh
source ${BASHDIR}/variables/completion.sh
source ${BASHDIR}/variables/eza.sh
source ${BASHDIR}/variables/fzf.sh
source ${BASHDIR}/variables/history.sh
source ${BASHDIR}/variables/man.sh
source ${BASHDIR}/variables/asdf.bash
source ${BASHDIR}/variables/direnv.bash
source ${BASHDIR}/variables/atuin.sh

# aliases
source ${BASHDIR}/aliases/general
source ${BASHDIR}/aliases/colorful.sh
source ${BASHDIR}/aliases/git
source ${BASHDIR}/aliases/eza
source ${BASHDIR}/aliases/zoxide

path_preppend "${HOME}/.config/bin"
path_preppend "${HOME}/.local/bin"
path_preppend "${CARGO_HOME}/bin"
