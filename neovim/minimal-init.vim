hi Normal ctermbg=none
" Remove drawing background (fix issues with transparent terminals)

set ts=4 " Set tabstop to 4
set sw=0 " Set shift width to 0 (means equal to tabstop)

set cursorline
hi CursorLine ctermbg=black cterm=none
" Highlight the current line

set number
hi LineNr ctermfg=gray
" Show line numbers

au BufEnter * silent! lcd %:p:h
" Automatically change directory to the local file.

set splitright " Make splits open on the right
command! VTerm silent! lcd %:p:h | vnew +terminal
" Open terminal with :VTerm

vnoremap <leader>c y`>a = <C-r>=<C-r>"<CR>
" Calculate math expressions with \c in visual mode
