#!/bin/env bash

update_env() {
    echo "update enviroment..."
    sudo apt update 
}

install_deps() {
    echo "installing deps..."
    sudo apt install -y wget git 
    echo "done"
}

__select_file() {
    echo "select file"
    url="http://192.168.2.12:8000/ubuntu-pkgs"
    filename="/tmp/ubuntu-pkgs"
    if [ -f $filename ]; then
        rm -rf $filename
    fi
    curl $url -o $filename  
}

__install() {
    sudo apt install -y $1
}

__read_file() {
    echo "installing packages..."
    total=$(wc -l <$filename)
    i=0
    while IFS='' read -r line; do
        per=$((i * 100 / $total))
        echo "$per"
        __install $line 
        i=$((i + 1))
    done < "$filename" 
}

install_pkgs() {
    echo "install"
    __select_file
    echo "install"
    __read_file
}

fonts() {
    echo "installing fonts..."
    if [ ! -d ~/.local/share/fonts ]; then
        mkdir -p ~/.local/share/fonts
    fi

    echo "installing Fira Code"
    wget -q https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip -P /tmp
    unzip -o /tmp/Fira_Code_v6.2.zip -d ~/.local/share/fonts/Fira_Code_v6.2 > /dev/null

    echo "installing Iosevka"
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Iosevka.zip -P /tmp
    unzip -o /tmp/Iosevka.zip -d ~/.local/share/fonts/Iosevka > /dev/null

    echo "installing JetBrains"
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/JetBrainsMono.zip -P /tmp
    unzip -o /tmp/JetBrainsMono.zip -d ~/.local/share/fonts/JetBrainsMono > /dev/null

    echo "installing Ubuntu"
    wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Ubuntu.zip -P /tmp
    unzip -o /tmp/Ubuntu.zip -d ~/.local/share/fonts/Ubuntu > /dev/null

    echo "installing Awesome"
    wget -q https://github.com/FortAwesome/Font-Awesome/releases/download/6.1.1/fontawesome-free-6.1.1-desktop.zip -P /tmp
    unzip -o /tmp/fontawesome-free-6.1.1-desktop.zip -d ~/.local/share/fonts/Awesome > /dev/null

    echo "installing Devicons"
    wget -q https://github.com/vorillaz/devicons/archive/master.zip -P /tmp
    unzip -o /tmp/master.zip -d ~/.local/share/fonts/Devicons > /dev/null

    echo "updating font cache"
    fc-cache -f
    echo "done"
}

starship() {
    if [ -x starship ]; then
        echo "starship: already done"
    else
        echo "starship install"
        curl -sS https://starship.rs/install.sh | sh
    fi
}

vim_plugins() {
    mkdir -p "~/.config/nvim/autoload"
    curl -Ls "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > "~/.config/nvim/autoload/plug.vim"
}

tmux_plugins() {
    if [ -d ~/.tmux/plugins/tpm ]; then
        echo "tmux plugins: already done"	
    else
        echo "tmux plugins: installed"
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

asdf() {
    echo "dev tools"
    if [ -d ~/.asdf ]; then
        echo "asdf: already done"
    else
        echo "asdf: installed"
        git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.10.2
    fi
}

my_dotfiles() {
    echo "creating symbolic links"
    git clone https://github.com/robsonrod/dotfiles.git ~/dotfiles
    ~/dotfiles/stow alacritty bash dunst emacs gtk-3.0 most nvim others picom qtile i3 i3status-rust rofi starship terminator tmux utils zathura
}

exec_all() {
    update_env
    install_deps
    install_pkgs
    services
    fonts
    starship
    vim_plugins
    tmux_plugins
    asdf
    my_dotfiles
}

exec_all

