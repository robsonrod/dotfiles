SHELL := /bin/sh
.ONESHELL:

all: mandatory_tools term_tools dev_tools links

mandatory_tools:
	@echo "mandatory tools"
	@sudo pacman -Syy --noconfirm bash-completion alacritty wget zip unzip git exa ripgrep zoxide fzf tmux neovim terminator powerline-fonts
	@echo "done"

fonts:
	@echo "installing fonts..."
	if [ ! -d ~/.fonts ]; then
		@mkdir -p ~/.fonts
	fi

	@echo "installing Fira Code"
	@wget -q https://github.com/tonsky/FiraCode/releases/download/6.2/Fira_Code_v6.2.zip -P /tmp
	@unzip /tmp/Fira_Code_v6.2.zip -d ~/.fonts/Fira_Code_v6.2 > /dev/null

	@echo "installing Iosevka"
	@wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v2.1.0/Iosevka.zip -P /tmp
	@unzip /tmp/Iosevka.zip -d ~/.fonts/Iosevka > /dev/null

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
	@sudo pacman -Syy --noconfirm clang bear ccls

	@if [ -d ~/.asdf ]; then
		@echo "asdf: already done"
	@else
		@echo "asdf: installed"
		@git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.10.0
	@fi

create_dirs:
	@if [ ! -d ~/.config/alacritty ]; then
		@mkdir -p ~/.config/alacritty/themes
	@fi

	@if [ ! -d ~/.config/nvim ]; then
		@mkdir -p ~/.config/nvim
	@fi

	@if [ ! -d ~/.config/terminator ]; then
		@mkdir -p ~/.config/terminator
	@fi

links: create_dirs
	@echo "creating symbolic links"
	@ln -sfv ~/dotfiles/emacs/init.el ~/.emacs
	@mv ~/.bashrc ~/.bashrc.old
	@ln -sfv ~/dotfiles/bashrc/bashrc ~/.bashrc
	@ln -sfv ~/dotfiles/bashrc/bash_custom ~/.bash_custom
	@ln -sfv ~/dotfiles/terminator/config ~/.config/terminator/config
	@ln -sfv ~/dotfiles/tmux/tmux.conf ~/.tmux.conf
	@ln -sfv ~/dotfiles/alacritty/alacritty.yml ~/.config/alacritty/alacritty.yml
	@ln -sfv ~/dotfiles/alacritty/themes/dracula.yml ~/.config/alacritty/themes/dracula.yml
	@ln -sfv ~/dotfiles/nvim/init.vim ~/.config/nvim/init.vim
	@ln -sfv ~/dotfiles/nvim/coc-settings.json ~/.config/nvim/coc-settings.json
	@ln -sfv ~/dotfiles/starship/starship.toml ~/.config/starship.toml
