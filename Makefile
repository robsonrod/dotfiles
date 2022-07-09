SHELL := /bin/sh
.ONESHELL:

all: mandatory_tools fonts term_tools dev_tools links

mandatory_tools:
	@echo "mandatory tools"
	@sudo xbps-install -S bash-completion wget zip unzip git exa ripgrep zoxide fzf tmux terminator wmctrl bat alacritty ImageMagick firefox
	@echo "done"

fonts:
	@echo "installing fonts..."
	if [ ! -d ~/.local/share/fonts ]; then
		@mkdir -p ~/.local/share/fonts
	fi

	@echo "installing Fira Code"
	@wget -q https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip -P /tmp
	@unzip /tmp/Fira_Code_v6.2.zip -d ~/.local/share/fonts/Fira_Code_v6.2 > /dev/null

	@echo "installing Iosevka"
	@wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Iosevka.zip -P /tmp
	@unzip /tmp/Iosevka.zip -d ~/.local/share/fonts/Iosevka > /dev/null

	@echo "installing Awesome"
	@wget -q https://github.com/FortAwesome/Font-Awesome/releases/download/6.1.1/fontawesome-free-6.1.1-desktop.zip -P /tmp
	@unzip /tmp/fontawesome-free-6.1.1-desktop.zip -d ~/.local/share/fonts/Awesome > /dev/null

	@echo "installing Devicons"
	@wget -q https://github.com/vorillaz/devicons/archive/master.zip -P /tmp
	@unzip /tmp/master.zip -d ~/.local/share/fonts/Devicons > /dev/null

	@echo "updating font cache"
	@fc-cache -f
	@echo "done"

term_tools:
	@if [ -x starship ]; then
		@echo "starship: already done"
	@else
		@echo "starship install"
		@curl -sS https://starship.rs/install.sh | sh
	@fi

	@if [ -d ~/.tmux/plugins/tpm ]; then
		@echo "tmux plugins: already done"	
	@else
		@echo "tmux plugins: installed"
		@git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
	@fi

dev_tools:
	@echo "dev tools"
	@sudo xbps-install -S base-devel clang Bear cmake 

	@if [ -d ~/.asdf ]; then
		@echo "asdf: already done"
	@else
		@echo "asdf: installed"
		@git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.10.0
	@fi

do_config:
	@if [ -d ~/.config ]; then
		@mv ~/.config ~/.config.old
	@fi

	@ln -sfv ~/dotfiles/.config ~/.config

links: do_config
	@echo "creating symbolic links"
	@ln -sfv ~/dotfiles/emacs/init.el ~/.emacs
	@mv ~/.bashrc ~/.bashrc.old
	@ln -sfv ~/dotfiles/bashrc/bashrc ~/.bashrc
	@ln -sfv ~/dotfiles/.Xresources ~/.Xresources
	@ln -sfv ~/dotfiles/.xinitrc ~/.xinitrc
	@ln -sfv ~/dotfiles/tmux/tmux.conf ~/.tmux.conf

copy_usb_perms:
	@sudo mkdir -p /etc/polkit-1/localauthority/50-local.d
	@sudo cp ./utils/10-udisks.pkla /etc/polkit-1/localauthority/50-local.d/10-udisks.pkla
