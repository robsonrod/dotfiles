#!/usr/bin/env bash

setup() {
	DIR="$(cd "$(dirname $0)" && pwd)"

	echo "setting work directory"
	WORKDIR=$(mktemp -d -p "$dir")
	echo "working dir: $WORK_DIR"

	echo "copying packages to working dir"
	cp -v $DIR/pkglist.txt $WORKDIR/pkglist.txt
}

cleanup() {
	echo "Deleting work directory: $WORKDIR"
	rm -rf $WORKDIR
}

update_keyring() {
	echo "Update keyring"
	sudo pacman -Sy --needed archlinux-keyring --noconfirm
	echo "done"
}

update_mirror() {
	echo "Add fast mirror"
	sudo cp /etc/pacman.d/mirrorlist /etc/pacman.d/mirrorlist.bkp
	sudo reflector -c BR --latest 5 --age 2 --fastest 5 --protocol https --sort rate --save /etc/pacman.d/mirrorlist
	echo "done"
}

update_base_packages() {
	echo "Update mirrors"
	sudo pacman -Syy
	echo "done"

	echo "Update all packages"
	sudo pacman -Syu --noconfirm --needed --overwrite '*'
	echo "done"
}

install_paru() {
	echo "installing paru"
	paru_dir=$WORKDIR/paru
	sudo pacman -S --needed git base-devel
	command -v paru &>/dev/null || git clone https://aur.archlinux.org/paru-bin.git $paru_dir
	cd $paru_dir &>/dev/null && makepkg --needed -si && rm -rf $paru_dir || true
	echo "done"
}

install_packages() {

	if [ ! -f $WORKDIR/pkglist.txt ]; then
		echo "file pkglist.txt not found in $WORKDIR"
		exit 1
	fi

	paru -Sy --needed - <$WORKDIR/pkglist.txt
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

enable_services() {
	sudo systemctl enable sshd
	sudo systemctl enable cronie
	sudo systemctl enable NetworkManager
	sudo systemctl enable systemd-timesyncd
}

enable_darkmode_gtk4() {
	gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
}

setup
update_keyring
update_mirror
install_paru
update_base_packages
install_packages
install_tmux_plugins
install_asdf
enable_services
enable_darkmode_gtk4
cleanup
