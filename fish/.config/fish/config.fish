function fish_title
    true
end

function fish_greeting
end

if test -n "$EMACS"
    set -x TERM eterm-color
end

# system admin
set OS_ID (awk -F= ' /^ID=/ { gsub("\"", ""); print $2 } ' /etc/os-release)
if test $OS_ID = arch
    abbr -a i 'sudo pacman -Sy'
    abbr -a u 'sudo pacman -Syy'
    abbr -a up 'sudo pacman -Suy '
    abbr -a r 'sudo pacman -Rs'
    abbr -a cat bat
else
    abbr -a i 'sudo apt-get install'
    abbr -a u 'sudo apt-get update'
    abbr -a up 'sudo apt-get upgrade'
    abbr -a r 'sudo apt-get remove'
    abbr -a cat batcat
    abbr -a fd fdfind
end

# general aliases
abbr -a c clear
abbr -a e exit
abbr -a g git
abbr -a h history
abbr -a nv nvim
abbr -a em emacs -nw
abbr -a o xdg-open
abbr -a m make
abbr -a gc 'git checkout'
abbr -a gb 'git checkout -b'
abbr -a gcl 'git clone'
abbr -a meminfo 'free -m -l -t'
abbr -a top btm

abbr -a .. 'cd ..'
abbr -a 2. 'cd ../..'
abbr -a 3. 'cd ../../..'
abbr -a 4. 'cd ../../../..'
abbr -a 5. 'cd ../../../../..'

abbr -a cp 'cp -i'
abbr -a mv 'mv -i'
abbr -a rm 'rm -i'
abbr -a gh 'history|grep'
abbr -a mnt 'mount | awk -F' ' '{ printf \"%s\t%s\n\",\$1,\$3; }' | column -t | egrep ^/dev/ | sort'
abbr -a cpv 'rsync -ah --info=progress2'
abbr -a ln 'ln -i'
abbr -a lns 'ln -si'

abbr -a compress 'tar -czf'
abbr -a untar 'tar -xvzf'

abbr -a s systemctl
abbr -a suser 'systemctl --user'

fish_config theme choose 'Dracula Official'

starship init fish | source
zoxide init fish | source
source $HOME/.asdf/asdf.fish

abbr -a wget 'wget --hsts-file="$XDG_DATA_HOME/wget-hsts" -c'

if command -v exa >/dev/null
    abbr -a l 'exa -l --color=always --group-directories-first'
    abbr -a ls 'exa --color=always --group-directories-first'
    abbr -a la 'exa -la --color=always --octal-permissions --group-directories-first -g --icons'
    abbr -a ll 'exa -l --color=always --octal-permissions --group-directories-first'
    abbr -a tree 'exa --tree'
else
    abbr -a l ls
    abbr -a ls 'ls -l'
    abbr -a la 'ls -la'
    abbr -a ll 'ls -la'
end

set -Ux GDK_SCALE 2
set -Ux GDK_DPI_SCALE 0.5
set -Ux QT_AUTO_SCREEN_SET_FACTOR 0
set -Ux QT_SCALE_FACTOR 2
set -Ux QT_FONT_DPI 96

set -Ux LESS_TERMCAP_mb \e'[1;31m'
set -Ux LESS_TERMCAP_md \e'[1;34m'
set -Ux LESS_TERMCAP_so \e'[01;45;37m'
set -Ux LESS_TERMCAP_us \e'[1;36m'
set -Ux LESS_TERMCAP_me \e'[0m'
set -Ux LESS_TERMCAP_se \e'[0m'
set -Ux LESS_TERMCAP_ue \e'[0m'

set -Ux EXA_COLORS "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:"

set -Ux FZF_DEFAULT_OPTS '--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'
set -Ux FZF_DEFAULT_COMMAND 'rg --files --follow --no-ignore-vcs --hidden -g "!{node_modules/,.git/,.venv/}"'

set -g fish_prompt_pwd_dir_length 3
set __fish_git_prompt_showupstream none
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showdirtystate ''

function d --description "Goto root project"
    while test $PWD != /
        if test -d .git
            break
        end
        cd ..
    end
end

function sudo --description "Exec sudo !! like bash"
    if test "$argv" = !!
        echo sudo $history[1]
        eval command sudo $history[1]
    else
        command sudo $argv
    end
end

function ssh-fzf --description "List hosts to connect"
    set selected (rg "Host " ~/.ssh/config | awk '{print $2}' | fzf --query "$LBUFFER" --height 30%)
    if test ! -z "$selected"
        ssh -X "$selected"
    end
end

function vs --description "Start VPN service"
    vpn.sh start
end

function vf --description "Finish VPN service"
    vpn.sh stop
end

function reload --description "Reload fish without restart terminal"
    source $HOME/.config/fish/config.fish
end

function fish_command_not_found
    __fish_default_command_not_found_handler $argv
end
