call plug#begin()

Plug 'PeterRincker/vim-argumentative'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'alvan/vim-closetag'
Plug 'aperezdc/vim-template'
Plug 'dhruvasagar/vim-table-mode'
Plug 'jiangmiao/auto-pairs'
Plug 'jistr/vim-nerdtree-tabs'
Plug 'scrooloose/nerdtree'
Plug 'tpope/vim-surround'

Plug 'chriskempson/base16-vim'
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
Plug 'zchee/deoplete-clang'
Plug 'zchee/deoplete-go', { 'do': 'make' }

call plug#end()

" Settings
set ts=4
set sw=0
set cursorline
set number
set splitright
set nrformats=alpha,octal,hex
set mouse=a

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
au FileType yaml,markdown setlocal ts=2 expandtab indentkeys=

au User LanguageClientStarted set signcolumn=yes
au User LanguageClientStopped set signcolumn=auto

" Plugins
"
let g:AutoPairsMapBS = 0
let g:AutoPairsMultilineClose = 0

let g:LanguageClient_autoStart = 1
let g:LanguageClient_serverCommands = {
	\ 'rust': ['rustup', 'run', 'nightly', 'rls'],
\ }

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme = 'base16_default'

let g:deoplete#enable_at_startup = 1

let g:nerdtree_tabs_open_on_console_startup = 1

let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1
let g:rust_recommended_style = 0

" Commands
function! JSON()
	%!python -m json.tool
	retab!
endfunction
function! JavaFmt()
	silent %!java -jar ~/.config/nvim/google-java-format-1.3-all-deps.jar -
	let tab = &ts
	let &ts=2
	retab!
	let &ts=tab
endfunction
function! InjectSemicolon()
	let pos = getpos(".")
	normal! A;
	call setpos(".", pos)
endfunction

command! CWD silent! lcd %:p:h | echo "Changed directory!"
command! Term silent! lcd %:p:h | tabe +terminal
command! VTerm silent! lcd %:p:h | vnew +terminal
command! SudoW silent exec "w !sudo tee %" | echo "Saved!"

command! JSON call JSON()
command! JSONMIN silent %!ruby ~/.config/nvim/minify.rb
command! JavaFmt call JavaFmt()
command! -nargs=1 Keyword syn keyword Keyword <args>

nnoremap <leader>; :call InjectSemicolon()<CR>
nnoremap <leader>n :NERDTree \| wincmd p<CR>
vnoremap <leader>c y`>a = <c-r>=<c-r>"<cr><esc>
tnoremap <esc><esc> <c-\><c-n>
