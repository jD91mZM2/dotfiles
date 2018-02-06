" Remove drawing background (fix issues with transparent terminals)
hi Normal ctermbg=none

" Mouse support
set mouse=a

" Tab settings
set expandtab " Turn all tabs into spaces
set sw=0 " Set shift width to 0 (means equal to tabstop)
set ts=4 " Set tabstop to 4

" Highlight the current line
set cursorline
hi CursorLine ctermbg=black cterm=none

" Show line numbers
set number
hi LineNr ctermfg=gray

" Automatically change directory to the local file.
au BufEnter * silent! lcd %:p:h

" Open terminals in different locations with :Term, :HTerm and :VTerm
set splitright " Make splits open on the right
command! Term  silent! lcd %:p:h | tabe +terminal
command! HTerm silent! lcd %:p:h | rightbelow new +terminal
command! VTerm silent! lcd %:p:h | vnew +terminal

" Calculate math expressions with \c in visual mode
vnoremap <LEADER>c y`>a = <C-R>=<C-R>"<CR><ESC>

" Go to normal mode using double escape in terminal mode
tnoremap <ESC><ESC> <C-\><C-N>
