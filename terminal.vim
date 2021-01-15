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

" Open Terminal in the correct pwd
function! s:terminal()
    let entry = ShellScript([
                \ [ 'cd', expand('%:p:h') ],
                \ 'exec "$0"',
                \ ])
    let shell = exists('$SHELL') ? $SHELL : "/bin/sh"
    let command = 'terminal ' . shell . ' -c ' . shellescape(entry)

    return 'rightbelow vsplit +' . escape(command, ' ')
endfunction
call Map('n', '<leader>S', function('s:terminal'))

" Map navigation keys
for m in ['h', 'j', 'k', 'l']
    call MapKeys('t', '<C-w>' . m, '<C-w>' . m)
endfor
