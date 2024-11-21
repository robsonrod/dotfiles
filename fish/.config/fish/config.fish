function fish_title
    true
end

function fish_greeting
end

set -gx EDITOR nvim
set -gx GPG_TTY (tty)
set -gx COLORTERM truecolor
set -gx TERM xterm-256color

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

fish_config theme choose 'Catppuccin Macchiato'

if command -v eza >/dev/null
    alias l 'eza -l --color=always --group-directories-first --git'
    alias ls 'eza --color=always --group-directories-first --git'
    alias la 'eza -la --color=always --octal-permissions --group-directories-first -g --icons --git'
    alias ll 'eza -l --color=always --octal-permissions --group-directories-first --git'
    alias lt 'eza --tree --level=3 --long --icons --git'
else
    alias l ls
    alias ls 'ls -l'
    alias la 'ls -la'
    alias ll 'ls -la'
end


if test "$(uname -a | cut -d ' ' -f2)" = "iracema"
    set -Ux GDK_SCALE 2
    set -Ux GDK_DPI_SCALE 0.5
    set -Ux QT_AUTO_SCREEN_SET_FACTOR 0
    set -Ux QT_SCALE_FACTOR 2
    set -Ux QT_FONT_DPI 96
end

# system admin
if command -v paru > /dev/null
	abbr -a p 'paru'
	abbr -a up 'paru -Syu'
    abbr -a i 'paru -Sy'
    abbr -a r 'paru -Rs'
else
	abbr -a p 'sudo pacman'
	abbr -a up 'sudo pacman -Syu'
    abbr -a i 'sudo pacman -Sy'
    abbr -a r 'sudo pacman -Rs'
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
abbr -a cdev 'ssh dev'

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
abbr -a btw 'macchina'

set -Ux FZF_DEFAULT_OPTS "\
--color=bg+:#363a4f,bg:#24273a,spinner:#f4dbd6,hl:#ed8796 \
--color=fg:#cad3f5,header:#ed8796,info:#c6a0f6,pointer:#f4dbd6 \
--color=marker:#b7bdf8,fg+:#cad3f5,prompt:#c6a0f6,hl+:#ed8796 \
--color=selected-bg:#494d64 \
--multi"
set -Ux FZF_DEFAULT_COMMAND 'rg --files --follow --no-ignore-vcs --hidden -g "!{node_modules/,.git/,.venv/}"'
set -Ux LS_COLORS $(vivid generate catppuccin-macchiato)

set -g fish_prompt_pwd_dir_length 3
set __fish_git_prompt_showupstream none
set __fish_git_prompt_showuntrackedfiles yes
set __fish_git_prompt_showdirtystate yes
set __fish_git_prompt_showdirtystate ''
set sponge_purge_only_on_exit true

# bind to ctrl-r in normal and insert mode, add any other bindings you want here too
bind \cr _atuin_search
bind -M insert \cr _atuin_search

function fish_command_not_found
    __fish_default_command_not_found_handler $argv
end

set fish_function_path $fish_function_path "$HOME/.config/fish/functions/work"
