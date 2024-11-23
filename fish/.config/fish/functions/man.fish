function man --wraps man --description "Display manual pages"

    set -lx LESS_TERMCAP_mb \e'[1;31m'
    set -lx LESS_TERMCAP_md \e'[1;33m'
    set -lx LESS_TERMCAP_so \e'[01;44;37m'
    set -lx LESS_TERMCAP_us \e'[1;37m'
    set -lx LESS_TERMCAP_me \e'[0m'
    set -lx LESS_TERMCAP_se \e'[0m'
    set -lx LESS_TERMCAP_ue \e'[0m'
    set -lx LESS '-R --use-color -Ddr'
    set -lx GROFF_NO_SGR yes

    set -lx MANPATH (string join : $MANPATH)
    if test -z "$MANPATH"
        type -q manpath and set MANPATH (command manpath)
    end

    command man $argv

end
