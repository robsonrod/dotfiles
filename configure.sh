#!/usr/bin/env bash
#
install_repositories() {
    echo "install repositories"

    echo "- install emacs repository"
    sudo add-apt-repository ppa:kelleyk/emacs
    sudo apt update

    echo "- install neovim repository"
    sudo add-apt-repository ppa:neovim-ppa/unstable
    sudo apt update

    echo "- install peek repository"
    sudo add-apt-repository ppa:peek-developers/stable
    sudo apt update

    [[ -f "/etc/os-release" ]] && ID="$(awk -F= ' /^ID=/ { gsub("\"", ""); print $2 } ' /etc/os-release)"
    if [ "$ID" = "ubuntu" ];
        echo "- install alacritty repository"
        sudo add-apt-repository ppa:mmstick76/alacritty
        sudo apt update
    fi
}

install_packages() {
    echo "- install packages"
    dir=$(dirname $0)
    filename=${dir}/packages.txt

    echo "sudo apt install " $(grep -vE "^\s*#" ${filename}  | tr "\n" " ")
}

install_fonts() {
    echo "- install fonts"

    fonts_dir="$HOME/.local/share/fonts"
    if [ ! -d ~/.local/share/fonts ]; then 
        mkdir -p ${fonts_dir}
    fi

    dir=$(dirname $0)
    filename=${dir}/fonts.txt

    while read data
    do
        fontpkg=${data##*/}
        file="${fontpkg%.*}"
        echo "installing ${file}"
        wget -q $data -P /tmp
        unzip /tmp/$fontpkg -d "$fonts_dir/$file" > /dev/null
        rm -rf /tmp/$fontpkg
    done < $filename

    echo "updating font cache"
    fc-cache -f
    echo "done"
}

install_startship() {
    if [ ! -x starship ]; then
        echo "starship: already done"
        return
    fi

    curl -sS https://starship.rs/install.sh | sh
    echo "starship: installed"
}

install_tmux_plugins() {

    if [ -d $HOME/.tmux/plugins/tpm ]; then
        echo "tmux plugins: already done"	
        return
    fi

    git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
    echo "tmux plugins: installed"
}

install_asdf() {
    echo "install asdf"

    if [ -d $HOME/.asdf ]; then
        echo "asdf: already done"
        return
    fi

    git clone https://github.com/asdf-vm/asdf.git $HOME/.asdf --branch v0.10.2
    echo "asdf: installed"
}

install_packages
install_fonts
install_startship
install_tmux_plugins
install_asdf
