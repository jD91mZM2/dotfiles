call plug#begin()

Plug 'aperezdc/vim-template'
Plug 'jiangmiao/auto-pairs'

" Languages
Plug 'fatih/vim-go'
Plug 'vim-ruby/vim-ruby'

call plug#end()

" Settings
set tabstop=4
set indentkeys-=:
set shiftwidth=4
set cursorline
set number
set ruler

colorscheme ron
hi CursorLine ctermbg=darkgray cterm=none
hi LineNr ctermfg=gray
hi Trail ctermbg=red
match Trail /\s\+$/

let g:go_fmt_autosave = 0
let g:go_template_autocreate = 0

" Commands
command! CWD silent exec "cd %:p:h" | echo "Changed directory!"
command! VTerm rightbelow vnew +terminal
