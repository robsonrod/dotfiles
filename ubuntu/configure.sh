#!/usr/bin/env bash

ppa_exists() {
	ppa=$(find /etc/apt/ -name *.list | xargs cat | grep ^[[:space:]]*deb | awk -F'[/:]+' '{ sub("^www", "", $2); print $3 }' | grep $1)
	if [ ! -z "${ppa}" ]; then
		echo "1"
		return
	fi

	echo "0"
}

install_repositories() {
	echo "install repositories"

	exists=$(ppa_exists "kelleyk")
	if [ "$exists" == "0" ]; then
		echo "- install emacs repository"
		sudo add-apt-repository ppa:kelleyk/emacs -y >/dev/null 2>&1
		sudo apt-get update -qq
	else
		echo "- emacs-ppa repository: already added"
	fi

	exists=$(ppa_exists "neovim-ppa")
	if [ "$exists" == "0" ]; then
		echo "- install neovim repository"
		sudo add-apt-repository ppa:neovim-ppa/unstable -y >/dev/null 2>&1
		sudo apt-get update -qq
	else
		echo "- neovim-ppa: already added"
	fi

	exists=$(ppa_exists "peek-developers")
	if [ "$exists" == "0" ]; then
		echo "- install peek repository"
		sudo add-apt-repository ppa:peek-developers/stable -y >/dev/null 2>&1
		sudo apt-get update -qq
	else
		echo "- peek-ppa: already added"
	fi

	[[ -f "/etc/os-release" ]] && ID="$(awk -F= ' /^ID=/ { gsub("\"", ""); print $2 } ' /etc/os-release)"
	if [ "$ID" == "ubuntu" ]; then
		exists=$(ppa_exists "aslatter")
		if [ "$exists" == "0" ]; then
			echo "- install alacritty repository"
			sudo add-apt-repository ppa:aslatter/ppa -y >/dev/null 2>&1
			sudo apt-get update -qq
		else
			echo "- alacritty-ppa: already added"
		fi
	fi
}

install_docker_repository() {
	echo "install docker repository"

	if [ ! -f "/etc/apt/keyrings/docker.gpg" ]; then
		sudo install -m 0755 -d /etc/apt/keyrings
		curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /etc/apt/keyrings/docker.gpg
		sudo chmod a+r /etc/apt/keyrings/docker.gpg

		echo "deb [arch="$(dpkg --print-architecture)" signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu "$(. /etc/os-release && echo "$VERSION_CODENAME")" stable" | sudo tee /etc/apt/sources.list.d/docker.list >/dev/null

		sudo apt-get update -qq
	else
		echo "- docker: already installed"
	fi
}

install_packages() {
	echo "- install packages"
	dir=$(dirname $0)
	filename=${dir}/$1
	sudo apt-get install -qq $(grep -vE "^\s*#" ${filename} | tr "\n" " ") -y
}

install_fonts() {
	echo "- install fonts"

	fonts_dir="$HOME/.local/share/fonts"
	if [ ! -d ~/.local/share/fonts ]; then
		mkdir -p ${fonts_dir}
	fi

	dir=$(dirname $0)
	filename=${dir}/fonts.txt

	while read data; do
		fontpkg=${data##*/}
		file="${fontpkg%.*}"
		echo "installing ${file}"

		if [ ! -d "$fonts_dir/$file" ]; then
			wget -q $data -P /tmp
			unzip /tmp/$fontpkg -d "$fonts_dir/$file" >/dev/null
			echo "${file}: installed"
			rm -rf /tmp/$fontpkg
		else
			echo "${file}: alredy installed"
		fi
	done <$filename

	echo "updating font cache"
	fc-cache -f
	echo "done"
}

install_startship() {
	if [ -x starship ]; then
		echo "starship: already done"
		return
	fi

	wget -q https://starship.rs/install.sh -P /tmp
	chmod 0755 "/tmp/install.sh"
	/tmp/install.sh --yes >/dev/null
	echo "starship: installed"
}

install_tmux_plugins() {

	if [ -d "$HOME/.tmux/plugins/tpm" ]; then
		echo "tmux plugins: already done"
		return
	fi

	git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm
	echo "tmux plugins: installed"
}

install_asdf() {
	echo "install asdf"

	if [ -d "$HOME/.asdf" ]; then
		echo "asdf: already done"
		return
	fi

	git clone https://github.com/asdf-vm/asdf.git $HOME/.asdf --branch v0.10.2
	echo "asdf: installed"
}

install_rust() {
	echo "install rust"

	if [ -d "$HOME/./.rustup/toolchains" ]; then
		echo "rust: already done"
		return
	fi

	wget -q https://sh.rustup.rs -O /tmp/sh.rustup.rs
	chmod 0755 "/tmp/sh.rustup.rs"
	/tmp/sh.rustup.rs -y >/dev/null
	echo "rust: installed"
}

install_compose() {
	sudo groupadd docker

	sudo usermod -aG docker $USER
	sudo curl -sSL https://github.com/docker/compose/releases/download/v2.20.2/docker-compose-linux-x86_64 -o /usr/local/bin/docker-compose
	sudo chmod 755 /usr/local/bin/docker-compose
	sudo chown root:docker /usr/local/bin/docker-compose
}

usage() {
	echo -e "\nUsage:\n"
	echo "f - to install and full configuration"
}

check_user() {
	if [ -z $USER ]; then
		echo "Running in docker variable USER is not set"
		echo 'USER=$(whoami) ./configure.sh option '
		exit 1
	fi
}

main() {
	check_user
	case $1 in
	f)
		install_repositories
		install_docker_repository
		install_packages "full_packages.txt"
		install_fonts
		install_startship
		install_tmux_plugins
		install_asdf
		install_rust
		install_compose
		;;
	m)
		install_fonts
		install_startship
		install_tmux_plugins
		install_asdf
		install_rust
		;;
	*)
		usage
		exit 666
		;;
	esac
}

main "$1"
