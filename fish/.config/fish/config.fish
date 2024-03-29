function fish_title
    true
end

function fish_greeting
end

if status --is-interactive
    eval (gpgconf --launch gpg-agent)
end

if test -n "$EMACS"
    set -x TERM eterm-color
end

if test "$TERM" = "dumb"
    function fish_prompt
        echo "\$ "
    end
    function fish_right_prompt; end
    function fish_greeting; end
    function fish_title; end
end

# system admin
set OS_ID (awk -F= ' /^ID=/ { gsub("\"", ""); print $2 } ' /etc/os-release)
if test $OS_ID = arch
    abbr -a i 'sudo pacman -Sy'
    abbr -a u 'sudo pacman -Syy'
    abbr -a up 'sudo pacman -Suy '
    abbr -a r 'sudo pacman -Rs'
else
    abbr -a i 'sudo apt-get install'
    abbr -a u 'sudo apt-get update'
    abbr -a up 'sudo apt-get upgrade'
    abbr -a r 'sudo apt-get remove'
    alias bat 'batcat'
    alias fd 'fdfind'
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
abbr -a meminfo 'grc free -m -l -t'
abbr -a top btm

abbr -a add 'git add'
abbr -a addu 'git add -u'
abbr -a adda 'git add .'
abbr -a clone 'git clone'
abbr -a status 'git status'
abbr -a commit 'git commit -v -m'
abbr -a checkoutb 'git checkout -b'
abbr -a checkout 'git checkout'
abbr -a gdiff 'git diff'
abbr -a log 'git log --graph --pretty=format:\'%C(bold)%h%Creset%C(magenta)%d%Creset %s %C(yellow)<%an> %C(cyan)(%cr)%Creset\' --abbrev-commit --date=relative'
abbr -a grm 'git rm'
abbr -a gfetch 'git fetch --all --prune --verbose'
abbr -a greset 'git reset HEAD'
abbr -a pull 'git pull'
abbr -a ppush 'git pull && git push'
abbr -a push 'git push origin'
abbr -a gclear 'git clean -xfd'
abbr -a branchd 'git branch -D'
abbr -a master 'git checkout master'
abbr -a stashl 'git stash lis'
abbr -a stashp 'git stash pop'

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

if command -v exa >/dev/null
    alias l 'exa -l --color=always --group-directories-first'
    alias ls 'exa --color=always --group-directories-first'
    alias la 'exa -la --color=always --octal-permissions --group-directories-first -g --icons'
    alias ll 'exa -l --color=always --octal-permissions --group-directories-first'
    alias tree 'exa --tree'
else
    alias l ls
    alias ls 'ls -l'
    alias la 'ls -la'
    alias ll 'ls -la'
end

set -gx GPG_TTY (tty)
set -Ux GDK_SCALE 2
set -Ux GDK_DPI_SCALE 0.5
set -Ux QT_AUTO_SCREEN_SET_FACTOR 0
set -Ux QT_SCALE_FACTOR 2
set -Ux QT_FONT_DPI 96

set -Ux EXA_COLORS "uu=36:gu=37:sn=32:sb=32:da=34:ur=34:uw=35:ux=36:ue=36:gr=34:gw=35:gx=36:tr=34:tw=35:tx=36:"

set -Ux FZF_DEFAULT_OPTS '--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'
set -Ux FZF_DEFAULT_COMMAND 'rg --files --follow --no-ignore-vcs --hidden -g "!{node_modules/,.git/,.venv/}"'

set -g fish_prompt_pwd_dir_length 3
set __fish_git_prompt_showupstream none
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showdirtystate ''

function fish_command_not_found
    __fish_default_command_not_found_handler $argv
end

set fish_function_path $fish_function_path "$HOME/.config/fish/functions/work"
