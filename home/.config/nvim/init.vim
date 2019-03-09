call plug#begin()

" Editing
Plug 'PeterRincker/vim-argumentative'
Plug 'SirVer/ultisnips'
Plug 'aperezdc/vim-template'
Plug 'dhruvasagar/vim-table-mode'
Plug 'floobits/floobits-neovim'
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
Plug 'terryma/vim-multiple-cursors'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

" Look and feel
Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'

Plug 'chriskempson/base16-vim'

Plug 'enricobacis/vim-airline-clock'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Completion and compiling
Plug 'Shougo/neco-vim' " dependency of ncm2-vim
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'make release' }
Plug 'ncm2/ncm2'
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-tmux'
Plug 'roxma/nvim-yarp' " dependency of ncm2
Plug 'w0rp/ale'

" Languages
Plug 'LnL7/vim-nix'
Plug 'Xe/lolcode.vim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'baskerville/vim-sxhkdrc'
Plug 'cespare/vim-toml'
Plug 'chr4/nginx.vim'
Plug 'dag/vim-fish'
Plug 'https://gitlab.redox-os.org/redox-os/ion-vim'
Plug 'lervag/vimtex'
Plug 'mxw/vim-jsx'
Plug 'ncm2/ncm2-jedi'
Plug 'ncm2/ncm2-tern', {'do': 'npm install'}
Plug 'ncm2/ncm2-vim'
Plug 'pangloss/vim-javascript'
Plug 'rust-lang/rust.vim'
Plug 'udalov/kotlin-vim'
Plug 'vim-ruby/vim-ruby'

call plug#end()

" Settings
set cursorline
set expandtab
set mouse=a
set nowrap
set nrformats=alpha,octal,hex
set number
set signcolumn=yes
set splitright
set sw=0
set ts=4

" Colors
set background=dark
colorscheme base16-default-dark

hi Normal ctermbg=none
hi CursorLine ctermbg=green cterm=none
hi LineNr ctermfg=darkgray
hi Trail ctermbg=red

" Auto commands
au BufWinEnter * match Trail /\s\+$/ " Copy pasted from http://vim.wikia.com/wiki/Highlight_unwanted_spaces
au BufEnter * silent! lcd %:p:h

au FileType fish compiler fish
au FileType haskell setlocal ts=2
au FileType rust compiler cargo
au FileType yaml,markdown setlocal ts=2 indentkeys=

" Plugins

let g:AutoPairsMapBS = 0
let g:AutoPairsMultilineClose = 0

let g:airline#extensions#clock#format = '%I:%M%p'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16_default'

" Disable a few linters, which most of them (but not all) are replaced with
" something else.
let g:ale_linters = {
    \ 'javascript': [],
    \ 'nix': [],
    \ 'rust': [],
\ }

let g:nerdtree_tabs_open_on_console_startup = 1
let g:rust_recommended_style = 0
let g:table_mode_corner_corner = '+'
let g:templates_directory = fnamemodify($MYVIMRC, ':p:h') . '/vim-templates'
let g:templates_no_builtin_templates = 1

let g:LanguageClient_loggingFile = '/tmp/nvim-lang-client.log'
let g:LanguageClient_loggingLevel = 'INFO'
let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls'],
    \ 'nix': ['nix-lsp']
\ }

" Enable ncm2
au BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect
" ^ Required, see ':help Ncm2PopupOpen' for abosolutely no explanation why
set shortmess+=c " disable 'The only match' message, for example
inoremap <expr> <CR> pumvisible() ? '<C-y><CR>' : '<CR>'
" ^ handle enter correctly when a popup menu is up

" Commands
function! JSON()
    %!python -m json.tool
    retab!
endfunction

command! CWD silent! lcd %:p:h | echo 'Changed directory!'
command! Term silent! lcd %:p:h | tabe +terminal
command! VTerm silent! lcd %:p:h | vnew +terminal
command! HTerm silent! lcd %:p:h | rightbelow new +terminal
command! SudoW silent exec 'w !sudo tee % > /dev/null' | echo 'Saved!'

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <LEADER>ji <Plug>(JavaComplete-Imports-Add)
tnoremap <ESC><ESC> <C-\><C-N>

nnoremap <LEFT> :echoerr 'No using arrow keys! Bad!'<CR>
nnoremap <RIGHT> :echoerr 'No using arrow keys! Bad!'<CR>
nnoremap <UP> :echoerr 'No using arrow keys! Bad!'<CR>
nnoremap <DOWN> :echoerr 'No using arrow keys! Bad!'<CR>
inoremap <LEFT> <NOP>
inoremap <RIGHT> <NOP>
inoremap <UP> <NOP>
inoremap <DOWN> <NOP>
cnoremap <UP> <NOP>
cnoremap <DOWN> <NOP>
