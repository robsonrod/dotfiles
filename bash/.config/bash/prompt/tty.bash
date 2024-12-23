parse_git_dirty() {
    local green="$(tput setaf 2 2>/dev/null || printf '')"
    local red="$(tput setaf 1 2>/dev/null || printf '')"
    local yellow="$(tput setaf 3 2>/dev/null || printf '')"
    local blue="$(tput setaf 4 2>/dev/null || printf '')"
    local no_color="$(tput sgr0 2>/dev/null || printf '')"

    local status="$(git status --porcelain 2> /dev/null)"
    if [[ $? -ne 0 ]]; then
        printf "";
        return;
    fi

    local renamed=$(echo "${status}" | grep -c "renamed:")
    local modified=$(echo "${status}" | grep -c " M")
    local staged=$(echo "${status}" | grep -c "^M")
    local untracked=$(echo "${status}" | grep -c "??")
    local deleted=$(echo "${status}" | grep -c "^D")
    local new_file=$(echo "${status}" | grep -c "^A")

    if [ -n $renamed ] && [ $renamed != "0" ]; then
        printf '%s' "${blue}  $renamed${no_color} ";
    else
        printf "";
    fi

    if [ -n $new_file ] && [ $new_file != "0" ]; then
        printf '%s' "${green}  ($new_file)${no_color}";
    else
        printf "";
    fi

    if [ -n $untracked ] && [ $untracked != "0" ]; then
        printf '%s' "${yellow}  ($untracked)${no_color}";
    else
        printf "";
    fi

    if [ -n $modified ] && [ $modified != "0" ]; then
        printf '%s' "${blue}  ($modified)${no_color}";
    else
        printf "";
    fi

    if [ -n $staged ] && [ $staged != "0" ]; then
        printf '%s' " ${green}󱓏 ($staged)${no_color}";
    else
        printf "";
    fi

    if [ -n $deleted ] && [ $deleted != "0" ]; then
        printf '%s' "${red} 󱀷 ($deleted)${no_color}";
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

    if [ -n "$VIRTUAL_ENV" ]; then
        printf " [pyenv]"
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

parse_prompt_symbol() {
    local exit_code="$?"
    local red="\033[38;5;9m"
    local white="\033[38;5;15m"
    local no_format="\033[0m"
    if [[ $exit_code -eq 0 ]]; then
        PROMPT_SYMBOL=$(echo -e "${white}${no_format}")
    else
        PROMPT_SYMBOL=$(echo -e "${red}${no_format}")
    fi
}

bash_prompt() {
    PROMPT_DIRTRIM=2
    PS1="\[$(tput bold)\]\[$(tput setaf 1)\][\[$(tput setaf 3)\]\u\[$(tput setaf 2)\]@\[$(tput setaf 4)\]\h \[$(tput setaf 5)\]\w\[$(tput setaf 1)\]]\[$(tput setaf 7)\]\\$ \[$(tput sgr0)\]"
    if [[ "$(tty)" == '/dev/pts/'* ]]; then
        PS1='\n\[$(tput bold)\]\[$(tput setaf 4)\]$(parse_ssh_connection)\[$(tput setaf 5)\]\w\[$(tput setaf 12)\]$(parse_direnv)$(parse_docker_env)\[$(tput setaf 147)\]$(parse_git_branch)\[$(tput setaf 3)\]$(parse_git_dirty) ${PROMPT_SYMBOL} \[$(tput sgr0)\]'
    fi
}

PROMPT_COMMAND='parse_prompt_symbol'
bash_prompt
unset -f bash_prompt
