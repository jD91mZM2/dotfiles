call plug#begin()

Plug 'aperezdc/vim-template'
Plug 'jiangmiao/auto-pairs'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'altercation/vim-colors-solarized'

" Languages
Plug 'fatih/vim-go'
Plug 'justmao945/vim-clang'
Plug 'vim-ruby/vim-ruby'

call plug#end()

" Settings
set tabstop=4
set shiftwidth=4
set cursorline
set number
set ruler

" Colors
syntax enable
set background=dark
colorscheme solarized

hi CursorLine ctermbg=black cterm=none
hi LineNr ctermfg=gray
hi Trail ctermbg=red
match Trail /\s\+$/

let g:go_fmt_autosave = 0
let g:go_template_autocreate = 0

" Commands
command! CWD silent exec "cd %:p:h" | echo "Changed directory!"
command! VTerm rightbelow vnew +terminal

function! JSON()
	%!python -m json.tool
	retab!
endfunction

command! JSON call JSON()
command! JSONMIN silent %!ruby minify.rb
