#### General ####

PATH="$HOME/.local/bin":"$HOME/bin":$PATH
PATH=$GOPATH/bin:/usr/local/go/bin:$PATH

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

# prompt gpg password
export GPG_TTY=$(tty)

# git alias
alias gcl='git clone'
alias ga='git add'
alias gaa='git add ./'
alias gs='git status'
alias gb='git checkout -b'
alias gd='git diff'
alias gl="git log --graph --pretty=format:'%C(bold)%h%Creset%C(magenta)%d%Creset %s %C(yellow)<%an> %C(cyan)(%cr)%Creset' --abbrev-commit --date=relative"
alias gcmt='git commit -v -m'
alias grm='git rm'
alias gfth='git fetch --all --prune --verbose'
alias grst='git reset HEAD'
alias gmrg="git merge"
alias gpll='git pull'
alias gplp='git pull && git push'
alias gpsh='git push'
alias gcll='git clean -xfd'
alias gbcm='git branch -m'
alias gbcd='git branch -D'
alias gchm='git checkout master'
alias glgs="glgr --stat"
alias gstb="git stash branch"
alias gstd="git stash drop"
alias gstl="git stash list"
alias gstp="git stash pop"

# general aliases
alias g='git'
alias h='history'
alias v='nvim'
alias e='emacs -nw'
alias cd='z'
alias cat='bat'
alias open='xdg-open'

alias ..='cd ..' 
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ls='exa --color=always --group-directories-first'
alias la='exa -la --color=always --group-directories-first'
alias ll='exa -l --color=always --group-directories-first'
alias lt='exa -aT --color=always --group-directories-first'
alias tree='exa --tree'
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'
alias gh='history|grep'
alias mnt="mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort"
alias cpv='rsync -ah --info=progress2'

## void linux commands ##
alias xbpi='sudo xbps-install -S'
alias xbpu='sudo xbps-install -u'
alias xbpq='sudo xbps-install -Rs'

## other aliases ##
alias tkill='tmux kill-server'
alias rebuildx='xrdb ~/.Xresources'
alias reload='exec $BASH'

ex ()
{
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)   tar xjf $1   ;;
            *.tar.gz)    tar xzf $1   ;;
            *.tar.xz)    tar xvf $1   ;;
            *.bz2)       bunzip2 $1   ;;
            *.rar)       unrar x $1     ;;
            *.gz)        gunzip $1    ;;
            *.tar)       tar xf $1    ;;
            *.tbz2)      tar xjf $1   ;;
            *.tgz)       tar xzf $1   ;;
            *.zip)       unzip $1     ;;
            *.Z)         uncompress $1;;
            *.7z)        7z x $1      ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

cm ()
{
    if [ -f $2 ] ; then
        case $1 in
            *.tar.bz2)   tar -cjvf $1 $2  ;;
            *.tar.gz)    tar -czvf $1 $2  ;;
            *.tar.xz)    tar Jfcv $1 $2  ;;
            *.bz2)       bzip2 -c $2 > $1  ;;
            *.gz)        gzip -c $2 > $1  ;;
            *.tar)       tar cf $1  $2  ;;
            *.tbz2)      tar cjf $1 $2  ;;
            *.tgz)       tar czf $1 $2  ;;
            *.zip)       zip -r $1 $2  ;;
            *.Z)         compress -c $2 > $1 ;;
            *.7z)        7z a $1  $2 ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

if [ -f ~/.bash_custom ]; then
    . ~/.bash_custom
fi

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

ls_alias() {
    alias | grep $1 | awk 'match($0,/(\w+)\s(\w+)=\x27(.*)\x27/,a){print a[2],"-",a[3]}' 
}

lsalgit() {
    ls_alias git
}

sshkr() {
    if [ -z $1 ]; then
        echo 'Invalid parameter';
    elif [ ! -f $1 ]; then
        echo 'File not found';
    else
        eval $(ssh-agent)
        ssh-add -q $1
        echo "Added";
    fi
}

mntloop() {
    if [ $1 != "fdisk" ]; then

        if [  $# -lt 3 ]; then
            echo "mountloop begin-partition file.img mount-point"
        else
            count=$(($1 * 512))
            sudo mount -o loop,offset=$count $2 $3
        fi
    else
        if [ ! -f $2 ]; then
            echo "File not found"
        else
            sudo fdisk -lu $2
        fi
    fi
}

skey() {
    ssh_key=$(ssh-get-key)
    eval $(ssh-agent) &>/dev/null
    SSH_ASKPASS="ssh-askpassword" ssh-add -q ~/.ssh/"${ssh_key}" < /dev/null
}

f() {
    local files
    IFS=$'\n' files=($(fzf --query="$1" --multi --select-1 --exit-0))
    [[ -n "$files" ]] && xdg-open "${files[@]}"
}

