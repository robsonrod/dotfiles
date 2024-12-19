parse_git_dirty() {
    STATUS="$(git status --porcelain 2> /dev/null)"
    if [[ $? -ne 0 ]]; then
        printf "";
        return;
    fi

    local renamed=$(echo "${STATUS}" | grep -c "renamed:")
    local modified=$(echo "${STATUS}" | grep -c " M")
    local staged=$(echo "${STATUS}" | grep -c "^M")
    local untracked=$(echo "${STATUS}" | grep -c "??")
    local deleted=$(echo "${STATUS}" | grep -c "^D")
    local new_file=$(echo "${STATUS}" | grep -c "^A")

    if [ -n $renamed ] && [ $renamed != "0" ]; then
        printf "$renamed";
    else
        printf "";
    fi

    if [ -n ${new_file} ] && [ ${new_file} != "0" ]; then
        printf "  ($new_file)";
    else
        printf "";
    fi

    if [ -n $untracked ]; then
        printf "  ($untracked)";
    else
        printf "";
    fi

    if [ -n $modified ]; then
        printf "  ($modified)";
    else
        printf "";
    fi

    if [ -n $staged ]; then
        printf " 󱓏 ($staged)";
    else
        printf "";
    fi

    if [ -n $deleted ] && [ $deleted != "0" ]; then
        printf " ($deleted)";
    else
        printf "";
    fi
}

parse_git_branch() {
    local name=$(git rev-parse --abbrev-ref HEAD 2> /dev/null)
    if [ ! -z $name ]; then
        printf "  $name"
    else
        printf ""
    fi
}

parse_direnv() {
    if [ -n "$DIRENV_DIR" ]; then
        printf " [env]"
    else
        printf ""
    fi
}

parse_docker_env() {
    if [ -f /.dockerenv ]; then
        printf " [docker]"
    else
        printf ""
    fi
}

parse_ssh_connection() {
    local hostname=$(uname -a | cut -d ' ' -f2)
    if [ -z ${SSH_TTY} ]; then
        printf ""
    else
        printf "$hostname "
    fi
}

bash_prompt() {
    export PROMPT_DIRTRIM=2
    #PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\W\[$(tput setaf 1)\]]\[$(tput setaf 2)\] \[$(tput sgr0)\]"
    #PS1='\[$(tput bold)\]\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\w\[$(tput setaf 12)\]$(parse_direnv)$(parse_docker_env)\[$(tput setaf 10)\]$(parse_git_branch)\[$(tput setaf 222)\]$(parse_git_dirty)\n\[$(tput setaf 2)\]❯ \[$(tput sgr0)\]'
    PS1='\n\[$(tput bold)\]\[$(tput setaf 4)\]$(parse_ssh_connection)\[$(tput setaf 5)\]\w\[$(tput setaf 12)\]$(parse_direnv)$(parse_docker_env)\[$(tput setaf 10)\]$(parse_git_branch)\[$(tput setaf 3)\]$(parse_git_dirty)\n\[$(tput setaf 2)\]❯ \[$(tput sgr0)\]'
}

bash_prompt
unset -f bash_prompt
