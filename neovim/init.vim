" Donald Luo's neovim configuration

set nocompatible
syntax on
set nowrap
set encoding=utf8

" Plugins
" ==============

call plug#begin('~/.local/share/nvim/plugged')

" General utility
Plug 'scrooloose/nerdtree'            " tree explorer
Plug 'majutsushi/tagbar'              " class outline viewer
Plug 'ervandew/supertab'              " allows you to use <Tab> for all your insert completion needs
"Plug 'SirVer/ultisnips'               " snippets solution
"Plug 'godlygeek/tabular'              " text alignment
Plug 'junegunn/fzf.vim'               " fzf for vim
Plug 'jeetsukumaran/vim-buffergator'  " list, select and switch between buffers
Plug 'kana/vim-textobj-user'          " create your own text objects
Plug 'kana/vim-textobj-entire'        " text objects for entire buffers
Plug 'tpope/vim-surround'             " quoting/parenthesizing made simple
Plug 'tpope/vim-eunuch'               " Vim sugar for the unix shell commands
Plug 'terryma/vim-multiple-cursors'   " Sublime Text style multiple selections
Plug 'jiangmiao/auto-pairs'           " insert or delete brackets, parens, quotes in pair

" Programming languages
Plug 'neoclide/coc.nvim', { 'branch': 'release' }  " intellisense engine, full language server protocol support as VSCode
Plug 'sheerun/vim-polyglot'           " a collection of language packs

" Git Support
Plug 'tpope/vim-fugitive'             " git wrapper
Plug 'kablamo/vim-git-log'            " view git log interactively
Plug 'airblade/vim-gitgutter'         " shows a git diff inteh gutter and stages/undoes hunks

" User Interface
Plug 'vim-scripts/Improved-AnsiEsc'
Plug 'itchyny/lightline.vim'
Plug 'arcticicestudio/nord-vim'
Plug 'ryanoasis/vim-devicons'

" All of your plugins must be added before the following line
call plug#end()
filetype plugin indent on

" Configurations
" ==============

set number
set ruler
set autoindent
set smartindent
set smarttab
set shiftwidth=2
set softtabstop=2
set tabstop=2
set expandtab
set list listchars=tab:\ \ ,trail:·   " Display tabs and trailing spaces visually
set laststatus=2
set cursorline
set showmatch
set incsearch
set hlsearch
set mouse=a
set completeopt=menuone,menu,longest
set wildmode=longest,list,full
set wildmenu
set noshowmode

" Appearance
set t_Co=256
set background=dark

if (has('termguicolors'))
  set termguicolors
endif

colorscheme nord

" lightline
let g:lightline = {
  \ 'colorscheme': 'nord'
  \ }

" fd
let mapleader = "\<space>"
inoremap fd <esc>
vnoremap fd <esc>
cnoremap fd <esc>
onoremap fd <esc>
if has('nvim')
  tnoremap fd <C-\><C-n>
endif
set timeout timeoutlen=500 ttimeoutlen=100

nnoremap <leader>w :w<CR>
vmap <leader>y "+y
vmap <leader>d "+d
nmap <leader>p "+p
nmap <leader>P "+P
vmap <leader>p "+p
vmap <leader>P "+P
nmap <leader><leader> V

map <leader>n :NERDTreeToggle<CR>
map <leader>m :TagbarToggle<CR>

" fzf-vim
nmap ; :Buffers<CR>
nmap <leader>t :Files<CR>
nmap <leader>r :tags<CR>
