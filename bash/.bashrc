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
bind 'set completion-ignore-case off'
bind 'TAB:menu-complete'

# If not running interactively, don't do anything
case $- in
*i*) ;;
*) return ;;
esac

export BASHDIR="${HOME}/.config/bash"

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
source ${BASHDIR}/functions/cls
source ${BASHDIR}/functions/cll
source ${BASHDIR}/functions/cla
source ${BASHDIR}/functions/truecolor
source ${BASHDIR}/functions/emg
source ${BASHDIR}/functions/emt
source ${BASHDIR}/functions/get_keyboard
source ${BASHDIR}/functions/set_keyboard
source ${BASHDIR}/functions/toggle_keyboard

# update PS1 variable
source ${BASHDIR}/prompt/startup
source ${BASHDIR}/prompt/custom_loader
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

# aliases
source ${BASHDIR}/aliases/general
source ${BASHDIR}/aliases/colorful.sh
source ${BASHDIR}/aliases/git
source ${BASHDIR}/aliases/eza
source ${BASHDIR}/aliases/zoxide

# Emacs EAT
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && source "$EAT_SHELL_INTEGRATION_DIR/bash"

path_preppend "${HOME}/.config/bin"
path_preppend "${HOME}/.local/bin"
path_preppend "${CARGO_HOME}/bin"

[ -f ${HOME}/.bash_work ] && source ${HOME}/.bash_work
