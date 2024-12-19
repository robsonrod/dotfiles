parse_git_dirty() {
    local green="$(tput setaf 2 2>/dev/null || printf '')"
    local red="$(tput setaf 1 2>/dev/null || printf '')"
    local yellow="$(tput setaf 3 2>/dev/null || printf '')"
    local blue="$(tput setaf 4 2>/dev/null || printf '')"
    local no_color="$(tput sgr0 2>/dev/null || printf '')"

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
    local green="$(tput setaf 2 2>/dev/null || printf '')"
    local red="$(tput setaf 1 2>/dev/null || printf '')"
    local no_color="$(tput sgr0 2>/dev/null || printf '')"
    if [ "$?" == "0" ]; then
        printf '%s' "${green}${no_color}"
    else
        printf '%s' "${red}${no_color}"
    fi
}

bash_prompt() {
    PS1='\n\[$(tput bold)\]\[$(tput setaf 4)\]$(parse_ssh_connection)\[$(tput setaf 5)\]\W\[$(tput setaf 12)\]$(parse_direnv)$(parse_docker_env)\[$(tput setaf 10)\]$(parse_git_branch)\[$(tput setaf 3)\]$(parse_git_dirty)\n$(parse_prompt_symbol) \[$(tput sgr0)\]'
}

bash_prompt
unset -f bash_prompt
