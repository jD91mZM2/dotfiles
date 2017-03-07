call plug#begin()

Plug 'aperezdc/vim-template'
Plug 'jiangmiao/auto-pairs'
Plug 'alvan/vim-closetag'

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
hi Normal ctermbg=none
match Trail /\s\+$/

" Plugins

let g:go_fmt_autosave = 0
let g:go_template_autocreate = 0
let g:airline_theme='solarized'

" Commands
command! CWD silent exec "cd %:p:h" | echo "Changed directory!"
command! VTerm cd %:p:h | rightbelow vnew +terminal
command! SudoW silent exec "w !sudo tee %" | echo "Saved!"

function! JSON()
	%!python -m json.tool
	retab!
endfunction

command! JSON call JSON()
command! JSONMIN silent %!ruby ~/.config/nvim/minify.rb " ~/minify.rb for normal vim users.
