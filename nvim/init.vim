" Don't try to be vi compatible
set nocompatible

" Helps force plugins to load correctly when it is turned back on below
filetype off

" Automatic installation
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \| PlugInstall --sync | source $MYVIMRC
\| endif

" Plugins
call plug#begin('~/.config/nvim/plugged')

Plug 'mhinz/vim-startify'

" Language server
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jackguo380/vim-lsp-cxx-highlight'
Plug 'sheerun/vim-polyglot'

" Language stuff
Plug 'pangloss/vim-javascript' 
Plug 'maxmellon/vim-jsx-pretty'
Plug 'jparise/vim-graphql'
Plug 'HerringtonDarkholme/yats.vim'
Plug 'rhysd/vim-clang-format'
Plug 'preservim/nerdcommenter'

" File explorer
Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'

" Auto close parens
Plug 'jiangmiao/auto-pairs'
Plug 'tpope/vim-surround'

" Themes
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'
Plug 'itchyny/lightline.vim'
Plug 'dracula/vim', { 'name': 'dracula' }
Plug 'joshdick/onedark.vim'
Plug 'ryanoasis/vim-devicons'

" Fuzzy file search
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" Buffer explorer
Plug 'jlanzarotta/bufexplorer'

" Switch files (*.h/*.c/*.cpp)
Plug 'vim-scripts/a.vim'

" Raimbow parentheses 
Plug 'frazrepo/vim-rainbow'

" Enable Doxygen plugin
Plug 'mrtazz/DoxygenToolkit.vim'

" Plugin to enable git integration
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'

" Enable tabs 
Plug 'zefei/vim-wintabs'
Plug 'zefei/vim-wintabs-powerline'

Plug 'mg979/vim-visual-multi', {'branch': 'master'}

" Golang support
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

" Terminal 
Plug 'voldikss/vim-floaterm'

call plug#end()

let mapleader = ","

" Turn on syntax highlighting
syntax on

" For plugins to load correctly
filetype plugin indent on
filetype on

" Security
set modelines=0

" Show line numbers
set number

" Blink cursor on error instead of beeping (grr)
set visualbell

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Autocomplete words
set complete+=kspell

" Indentation of previous line
set autoindent

" Intelligent indentation for C
set smartindent

" Configure tabwidth and insert spaces instead of tabs
set tabstop=4        " tab width is 4 spaces
set shiftwidth=4     " indent also with 4 spaces
set expandtab        " expand tabs to spaces

" Highlight matching braces
set showmatch

set hidden
:set nosmd   " short for 'showmode'
:set noru    " short for 'ruler'

" Search
set hlsearch     " highlight matches
set incsearch    " incremental searching
set ignorecase   " searches are case insensitive...
set smartcase    " ... unless they contain at least one capital letter

"### CONFIGS

"This unsets the "last search pattern" register by hitting return
nnoremap <CR> :noh<CR><CR>

colorscheme dracula 
set background=dark
set cursorline

" Set path variable to current directory (from which you launched vim)
" and to all directories under current directory recursively
set path+=**

let g:startify_lists = [
        \ { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
        \ { 'type': 'files',     'header': ['   MRU']            },
        \ { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
        \ { 'type': 'sessions',  'header': ['   Sessions']       },
        \ { 'type': 'commands',  'header': ['   Commands']       },
        \ ]


let g:startify_bookmarks = [
        \ {'c':'~/.config/nvim/init.vim'},
        \ {'p': '~/Projetos/teste'},
        \]

let g:startify_custom_header =
        \ startify#fortune#cowsay('', '═','║','╔','╗','╝','╚')

" C and Cpp extensions
let g:alternateExtensions_h = "c,cpp,cxx,cc,CC"
let g:alternateExtensions_H = "C,CPP,CXX,CC"
let g:alternateExtensions_cpp = "h,hpp"
let g:alternateExtensions_CPP = "H,HPP"
let g:alternateExtensions_c = "h"
let g:alternateExtensions_C = "H"
let g:alternateExtensions_cxx = "h"

" Enable rainbow
let g:rainbow_active = 1
let g:rainbow_guifgs = ['RoyalBlue3', 'DarkOrange3', 'DarkOrchid3', 'FireBrick']
let g:rainbow_ctermfgs = ['lightblue', 'lightgreen', 'yellow', 'red', 'magenta']

let g:NERDTreeGitStatusWithFlags = 1

" Close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" DoxygenToolkit
let g:DoxygenToolkit_briefTag_pre="@synopsis "
let g:DoxygenToolkit_paramTag_pre="@param "
let g:DoxygenToolkit_returnTag="@returns "
let g:DoxygenToolkit_blockHeader="===================================================================================================="
let g:DoxygenToolkit_blockFooter="===================================================================================================="
let g:DoxygenToolkit_authorName="Author <author@author.com>"
let g:DoxygenToolkit_licenseTag="GPL"

" C++ syntax highlighting
let g:cpp_class_scope_highlight = 1
let g:cpp_member_variable_highlight = 1
let g:cpp_class_decl_highlight = 1

let g:lightline = {
      \ 'colorscheme': 'onedark',
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ]
      \ },
      \ 'component_function': {
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

"### MAPPING 
let g:NERDCreateDefaultMappings = 1

" (F2, CTRL-S) Save File
nmap <c-s> :w<CR>
imap <c-s> <ESC>:w<CR>a
"imap <c-s> <c-o><c-s>

" (CTRL-Q) Close File
nmap <c-q> :q<CR>
imap <c-q> <c-o><c-q>

nmap <c-x> :vsplit<CR>

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" Fzf keybindings
nnoremap <C-f> :Files<CR>
nnoremap <silent> <Leader>b :Buffers<CR>
nnoremap <silent> <C-f> :Files<CR>
nnoremap <silent> <Leader>f :Rg<CR>
nnoremap <silent> <Leader>/ :BLines<CR>
nnoremap <silent> <Leader>' :Marks<CR>
nnoremap <silent> <Leader>g :Commits<CR>
nnoremap <silent> <Leader>H :Helptags<CR>
nnoremap <silent> <Leader>hh :History<CR>
nnoremap <silent> <Leader>h: :History:<CR>
nnoremap <silent> <Leader>h/ :History/<CR> 

" Clang format - auto formatting
let g:clang_format#command = 'clang-format-9'
let g:clang_format#detect_style_file = 1
let g:clang_format#auto_format = 1

" Buffer explorer (F12)
noremap <silent> <F12> <ESC>:BufExplorer<CR>
noremap <silent> <leader>be <ESC>:BufExplorer<CR>

" Terminal
let g:floaterm_keymap_toggle = '<Leader>ft'
nmap <leader>ft :FloatermToggle!<cr>
nmap <c-t> :FloatermNew nnn<cr>

" Change to *.cpp/*.h file (ALT+o)
nnoremap <silent> <M-o> <ESC>:A<CR>

" NERDTreeToggle
nmap <silent> <leader>nt :NERDTreeToggle<CR>

" (F6) Create doxygen comment
map <F6> <ESC>:Dox<CR>

" COC related stuff
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
if exists('*complete_info')
  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
else
  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Run the Code Lens action on the current line.
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" go-vim plugin specific commands
" Also run `goimports` on your current file on every save
" Might be be slow on large codebases, if so, just comment it out
let g:go_fmt_command = "goimports"

" Status line types/signatures.
let g:go_auto_type_info = 1

au filetype go inoremap <buffer> . .<C-x><C-o>
