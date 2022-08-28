#!/bin/bash

mandatory_tools() {
    echo "mandatory tools"
    sudo pacman -S stow bash-completion wget zip unzip git exa ripgrep zoxide fzf tmux terminator wmctrl bat alacritty imagemagick firefox
    echo "done"
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

term_tools() {
    if [ -x starship ]; then
        echo "starship: already done"
    else
        echo "starship install"
        curl -sS https://starship.rs/install.sh | sh
    fi

    if [ -d ~/.tmux/plugins/tpm ]; then
        echo "tmux plugins: already done"	
    else
        echo "tmux plugins: installed"
        git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    fi
}

dev_tools() {
    echo "dev tools"
    sudo pacman -S base-devel clang bear cmake 

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
