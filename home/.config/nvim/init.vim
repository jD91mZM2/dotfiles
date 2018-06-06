call plug#begin()

" Editing
Plug 'PeterRincker/vim-argumentative'
Plug 'SirVer/ultisnips'
Plug 'aperezdc/vim-template'
Plug 'dhruvasagar/vim-table-mode'
Plug 'honza/vim-snippets'
Plug 'jiangmiao/auto-pairs'
Plug 'mattn/emmet-vim'
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
Plug 'autozimu/LanguageClient-neovim', { 'branch': 'next', 'do': 'make release' }
Plug 'roxma/nvim-completion-manager'
Plug 'w0rp/ale'

" Languages
Plug 'Xe/lolcode.vim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'cespare/vim-toml'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'udalov/kotlin-vim'
Plug 'vim-ruby/vim-ruby'
Plug 'LnL7/vim-nix'

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

let g:airline#extensions#clock#format = "%I:%M%p"
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16_default'

" Disable linters for rust, we use LanguageClient-neovim
let g:ale_linters = {
    \ 'rust': []
\ }

let g:nerdtree_tabs_open_on_console_startup = 1
let g:rust_recommended_style = 0
let g:table_mode_corner_corner='+'

let g:LanguageClient_serverCommands = {
    \ 'rust': ['rls']
\ }

" Commands
function! JSON()
    %!python -m json.tool
    retab!
endfunction

command! CWD silent! lcd %:p:h | echo "Changed directory!"
command! Term silent! lcd %:p:h | tabe +terminal
command! VTerm silent! lcd %:p:h | vnew +terminal
command! HTerm silent! lcd %:p:h | rightbelow new +terminal
command! SudoW silent exec "w !sudo tee % > /dev/null" | echo "Saved!"

nnoremap <F5> :call LanguageClient_contextMenu()<CR>
nnoremap <LEADER>ji <Plug>(JavaComplete-Imports-Add)
tnoremap <ESC><ESC> <C-\><C-N>
