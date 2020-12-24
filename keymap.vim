let mapleader = ' '

function! Map(mode, key, command)
    if a:mode =~# 'n'
        execute 'nnoremap <silent> ' . a:key . ' :' . a:command . '<CR>'
    endif
    if a:mode =~# 'v'
        execute 'vnoremap <silent> ' . a:key . ' <C-c>:' . a:command . '<CR>gv'
    endif
    if a:mode =~# 'i'
        execute 'inoremap <silent> ' . a:key . ' <C-o>:' . a:command . '<CR>'
    endif
    if a:mode =~# 'c'
        execute 'cnoremap ' . a:key . ' <C-c>:' . a:command . '<CR>:<C-p>'
    endif
endfunction

call Map('nvi', '<Left>', 'echo "You must never use arrow keys!"')
call Map('nvi', '<Right>', 'echo "You must never use arrow keys!"')
call Map('nvic', '<Up>', 'echo "You must never use arrow keys!"')
call Map('nvic', '<Down>', 'echo "You must never use arrow keys!"')

call Map('n', '<C-S>', 'w')
call Map('n', 'gt', 'Buffers')

call Map('n', '<leader>H', 'split')
call Map('n', '<leader>V', 'vsplit')
call Map('n', '<leader>%', 'source %')
call Map('n', '<leader>1', 'only')
call Map('n', '<leader>q', 'q')
call Map('n', '<leader><leader>', 'Files')
call Map('n', '<leader>f', 'Ranger')
call Map('n', '<leader>bk', 'bdelete!')

call Map('n', '<leader>g', 'Git')

exec 'nnoremap <leader>/ :Rg '
nnoremap <leader>: q:
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
nnoremap <leader>. :e %:p:h/
nnoremap D 0d$
