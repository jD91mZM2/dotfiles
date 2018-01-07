call plug#begin()

Plug 'PeterRincker/vim-argumentative'
Plug 'alvan/vim-closetag'
Plug 'aperezdc/vim-template'
Plug 'dhruvasagar/vim-table-mode'
Plug 'jiangmiao/auto-pairs'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'roxma/nvim-completion-manager'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'

Plug 'chriskempson/base16-vim'
Plug 'enricobacis/vim-airline-clock'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Languages
Plug 'Xe/lolcode.vim'
Plug 'artur-shaik/vim-javacomplete2'
Plug 'autozimu/LanguageClient-neovim', { 'do': ':UpdateRemotePlugins' }
Plug 'cespare/vim-toml'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'udalov/kotlin-vim'
Plug 'vim-ruby/vim-ruby'
Plug 'zchee/deoplete-go', { 'do': 'make' }

call plug#end()

" Settings
set cursorline
set expandtab
set mouse=a
set nrformats=alpha,octal,hex
set number
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

au BufEnter * setlocal signcolumn=yes
au BufLeave * setlocal signcolumn=no

au FileType fish compiler fish
au FileType haskell setlocal ts=2
au FileType rust compiler cargo
au FileType yaml,markdown setlocal ts=2 indentkeys=

" Plugins
"
let g:AutoPairsMapBS = 0
let g:AutoPairsMultilineClose = 0

let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
	\ 'rust': ['rls']
\ }

let g:airline#extensions#clock#format = "%I:%M%p"
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16_default'

let g:nerdtree_tabs_open_on_console_startup = 1

let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
let g:rust_recommended_style = 0

let g:table_mode_corner_corner='+'

" Commands
function! JSON()
	%!python -m json.tool
	retab!
endfunction
function! InjectSemicolon()
	let pos = getpos(".")
	normal! A;
	call setpos(".", pos)
endfunction

command! CWD silent! lcd %:p:h | echo "Changed directory!"
command! Term silent! lcd %:p:h | tabe +terminal
command! VTerm silent! lcd %:p:h | vnew +terminal
command! HTerm silent! lcd %:p:h | rightbelow new +terminal
command! SudoW silent exec "w !sudo tee % > /dev/null" | echo "Saved!"

command! JSON call JSON()
command! JSONMIN silent %!ruby ~/.config/nvim/minify.rb
command! JavaFmt call JavaFmt()
command! -nargs=1 Keyword syn keyword Keyword <args>

nnoremap <LEADER>; :call InjectSemicolon()<CR>
nnoremap <LEADER>c :make check<CR>
nnoremap <LEADER>n :NERDTree \| wincmd p<CR>
tnoremap <ESC><ESC> <C-\><C-N>
vnoremap <LEADER>c y`>a = <C-R>=<C-R>"<CR><ESC>
