" Donald Luo's neovim configuration

set nocompatible
syntax on
set nowrap
set encoding=utf8

" Plugins
" ==============

call plug#begin('~/.local/share/nvim/plugged')

" Utility
Plug 'scrooloose/nerdtree'
Plug 'majutsushi/tagbar'
Plug 'ervandew/supertab'
Plug 'vim-scripts/BufOnly.vim'
Plug 'wesQ3/vim-windowswap'
Plug 'SirVer/ultisnips'
Plug 'godlygeek/tabular'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'benmills/vimux'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'gilsondev/searchtasks.vim'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'tpope/vim-dispatch'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-entire'
Plug 'editorconfig/editorconfig-vim'
Plug 'Lokaltog/vim-easymotion'

" Generic Programming Support
Plug 'sheerun/vim-polyglot'
Plug 'honza/vim-snippets'
Plug 'Townk/vim-autoclose'
Plug 'tomtom/tcomment_vim'
Plug 'maksimr/vim-jsbeautify'
Plug 'neomake/neomake'
Plug 'Shougo/vimproc.vim', { 'do': 'make' }

" Git Support
Plug 'kablamo/vim-git-log'
Plug 'gregsexton/gitv'
Plug 'tpope/vim-fugitive'

" Python
Plug 'zchee/deoplete-jedi'

" JavaScript
Plug 'jelera/vim-javascript-syntax'
Plug 'pangloss/vim-javascript'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'marijnh/tern_for_vim'

" Lisp / Clojure
Plug 'Raimondi/delimitMate'
Plug 'kovisoft/slimv'

" Vimscript
Plug 'Shougo/neco-vim'

" User Interface
Plug 'vim-scripts/Improved-AnsiEsc'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'NLKNguyen/papercolor-theme'
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

" Appearance
set t_Co=256
set background=dark

if (has('termguicolors'))
  set termguicolors
endif

colorscheme PaperColor

" Vim-Airline
let g:airline#extension#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'hybrid'
let g:hybrid_custom_term_colors = 1
let g:hybrid_reduced_contrast = 1
let g:airline_theme='papercolor'

" neomake
autocmd! BufWritePost * Neomake

" Vim-supertab
let g:SuperTabDefaultCompletionType = '<C-X><C-O>'

" Vim-UtilSnips
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<c-b>'
let g:UltiSnipsJumpBackwardTrigger='<c-z>'
let g:UltiSnipsEditSplit='vertical'

" deoplete
let g:deoplete#enable_at_startup = 1

" Mapping Keys and timeout

let mapleader = "\<space>"
inoremap fd <esc>
vnoremap fd <esc>
cnoremap fd <esc>
onoremap fd <esc>
if has('nvim')
  tnoremap fd <C-\><C-n>
endif
set timeout timeoutlen=500 ttimeoutlen=100
nnoremap <leader>o :CtrlP<CR>
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

" <TAB>: completion.
inoremap <expr><TAB>    pumvisible() ? '\<C-n>' : '\<TAB>'

" Editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp:/.*']

" delimitMate
au! FileType lisp,clojure let b:loaded_delimitMate=1

" CtrlP
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
if has('win32')
  set wildignore+=*\\tmp\\*,*.swp,*.zip,*.exe
else
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip
endif
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_use_caching = 0
if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
  let g:ctrlp_prompt_mapping = { 'AcceptSelection("e")': ['<space>', '<CR>', '<2-LeftMouse>'] }
endif

" Slimv
let g:slimv_swank_cmd = '! mate-terminal -e sbcl --load ~/.vim/bundle/slimv/slime/start-swank.lisp &'
