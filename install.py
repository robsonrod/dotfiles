#!/usr/bin/python3

import os;
import os.path;

home = os.environ['HOME']
vimrc = f'{home}/.config/nvim/init.vim'
bundle = f'{home}/.vim/bundle'
tmux_conf = f'{home}/.tmux.conf'
tmux_tpm = f'{home}/.tmux/plugins/tpm'
work_dir = os.getcwd()

os.system('sudo apt-get install git && sudo apt-get install neovim && sudo apt-get install tmux');

if os.path.exists(vimrc):
    os.system(f'mv {vimrc} {vimrc}.old')

os.system(f'ln -sf {work_dir}/vim/vimrc {vimrc}')

home = os.environ['HOME']
if os.path.exists(f'{home}/.vim/bundle'):
    print('Nothing todo')    
else:
    os.system(f'git clone https://github.com/VundleVim/Vundle.vim.git {bundle}/Vundle.vim')

os.system('nvim --headless +PluginInstall +qall 2>/dev/null')

if os.path.exists(tmux_conf):
    os.system(f'mv {tmux_conf} {tmux_conf}.old')

os.system(f'ln -sf {work_dir}/tmux/tmux.conf {tmux_conf}')

if os.path.exists(tmux_tpm):
    print('Nothing todo')
else:
    os.system(f'git clone https://github.com/tmux-plugins/tpm {tmux_tpm}')
