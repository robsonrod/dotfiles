#!/bin/env bash

set_os_type() {
    os_type=$(lsb_release -a | grep 'Distributor ID' | awk -F':' '{ print tolower($2)}' | sed -e 's/^[[:space:]]*//')
}

update_env() {
    echo "update enviroment..."
    if [ "$os_type" == "ubuntu" ]; then
        sudo apt update
    else
        sudo xbps-install -Suy
    fi
}

select_file() {
    if [ "$os_type" == "ubuntu" ]; then
        filename="/tmp/ubuntu-toinstall.dat"
        if [ -f $filename ]; then
            rm -rf $filename
        fi
        wget -q http://192.168.2.12:8000/home_machine_pkgs.dat -P /tmp
    else 
        filename="/tmp/home_machine_pkgs.dat"
        if [ -f $filename ]; then
            rm -rf $filename
        fi
        wget -q http://192.168.2.12:8000/home_machine_pkgs.dat -P /tmp
    fi
}

install_pkgs() {
    if [ "$os_type" == "ubuntu" ]; then
        sudo apt install $1
    else
        sudo xbps-install -Sy $1
    fi

}

read_file() {
    echo "installing packages..."
    total=$(wc -l <$filename)
    i=0
    while IFS='' read -r line; do
        per=$((i * 100 / $total))
        echo "$per"
        install_pkgs $line 
        i=$((i + 1))
    done < "$filename" 
}

mandatory_tools() {
    echo "installing minimal tools..."
    if [ $os_type == "void" ]; then 
        echo "mandatory tools"
        sudo xbps-install -Sy wget git 
        echo "done"
    fi
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
        git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.10.0
    fi
}

dotfiles() {
    echo "creating symbolic links"
    stow alacritty bash dunst emacs gtk-3.0 most nvim others picom qtile rofi starship terminator tmux utils
}

set_os_type 
update_env
mandatory_tools
select_file
read_file
fonts
starship
nvim_plugins
asdf
dotfiles
