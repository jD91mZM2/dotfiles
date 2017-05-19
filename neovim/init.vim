call plug#begin()

Plug 'aperezdc/vim-template'
Plug 'jiangmiao/auto-pairs'
Plug 'alvan/vim-closetag'

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'

" Languages
Plug 'fatih/vim-go'
Plug 'rust-lang/rust.vim'
Plug 'racer-rust/vim-racer'
Plug 'justmao945/vim-clang'

Plug 'dag/vim-fish'
Plug 'vim-ruby/vim-ruby'
Plug 'artur-shaik/vim-javacomplete2'

call plug#end()

" Settings
filetype plugin indent on

set tabstop=4
set shiftwidth=4
set cursorline
set number
set splitright
set nrformats=alpha,octal,hex
set autowrite

" Colors
syntax enable
set background=dark
colorscheme solarized

hi CursorLine ctermbg=black cterm=none
hi LineNr ctermfg=gray
hi Normal ctermbg=none

hi Trail ctermbg=red

" Copy pasted from http://vim.wikia.com/wiki/Highlight_unwanted_spaces
au BufWinEnter * match Trail /^ \s*\|\s\+$/

" Plugins
au FileType fish compiler fish

let g:go_template_autocreate = 0
let g:go_fmt_command = 'goimports'
let g:go_metalinter_autosave = 1

let g:rustfmt_autosave = 1
let g:rust_recommended_style = 0
let g:racer_cmd = '~/.cargo/bin/racer'
let g:racer_experimental_completer = 1

let g:airline_theme = 'solarized'

let g:AutoPairsMultilineClose = 0
let g:AutoPairsMapBS = 0

" Commands
function! JSON()
	%!python -m json.tool
	retab!
endfunction
function! JavaFmt()
	silent %!java -jar ~/.config/nvim/google-java-format-1.3-all-deps.jar -
	let tab = &tabstop
	let &tabstop=2
	retab!
	let &tabstop=tab
endfunction

command! CWD silent exec "cd %:p:h" | echo "Changed directory!"
command! VTerm cd %:p:h | vnew +terminal
command! SudoW silent exec "w !sudo tee %" | echo "Saved!"
command! JSON call JSON()
command! JSONMIN silent %!ruby ~/.config/nvim/minify.rb
command! JavaFmt call JavaFmt()
command! -nargs=1 Keyword syn keyword Keyword <args>
