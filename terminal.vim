function! s:maybeInsert()
    if expand('%') =~# 'term://.*'
        normal! i
    endif
endfunction

augroup s:terminal
    au!

    " Don't show "Terminal Closed" screen
    au TermClose * bdelete!

    " Make terminals pretty
    au TermOpen * setlocal norelativenumber nonumber

    " Automatically enter insert mode
    au TermOpen * normal! i
    au BufEnter * call s:maybeInsert()
augroup END

" Open Terminal
call Map('n', '<leader>S', 'rightbelow vsplit +terminal')

" Map navigation keys
for m in ['h', 'j', 'k', 'l']
    call MapKeys('t', '<C-w>' . m, '<C-w>' . m)
endfor
