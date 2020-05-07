#!/usr/bin/python3

import os;
import os.path;

home = os.environ['HOME']
vimrc = f'{home}/.config/nvim/init.vim'
bundle = f'{home}/.vim/bundle'
work_dir = os.getcwd()

os.system("sudo apt-get install git && sudo apt-get install python3 && sudo apt-get install neovim");

if os.path.exists(vimrc):
    os.system(f'mv {vimrc} {vimrc}.old')

os.system(f'ln -sf {work_dir}/vim/vimrc {vimrc}')

home = os.environ['HOME']
if os.path.exists(f'{home}/.vim/bundle'):
    print('Nothing todo')    
else:
    os.system('git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim')

os.system('nvim +PluginInstall +qall +silent')
