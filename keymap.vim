let mapleader = ' '

function! Map(opts, trigger, command)
    let extra = ''
    if a:opts =~# 'b'
        let extra .= '<buffer> '
    endif

    if a:opts =~# 'n'
        execute 'nnoremap <silent> ' . extra . a:trigger . ' :' . a:command . '<CR>'
    endif
    if a:opts =~# 'v'
        execute 'vnoremap <silent> ' . extra . a:trigger . ' <C-c>:' . a:command . '<CR>gv'
    endif
    if a:opts =~# 'i'
        execute 'inoremap <silent> ' . extra . a:trigger . ' <C-o>:' . a:command . '<CR>'
    endif
    if a:opts =~# 'c'
        execute 'cnoremap ' . a:trigger . ' <C-c>:' . a:command . '<CR>:<C-p>'
    endif
endfunction

let s:count = 0
function! MapKeys(opts, trigger, keys)
    let extra = ''
    if a:opts =~# 'b'
        let extra .= '<buffer> '
    endif

    if type(a:keys) == type("hello")
        exec 'nnoremap ' . extra . a:trigger . ' ' . a:keys
    elseif type(a:keys) == type(function('MapKeys'))
        " Assign stupid unique global name to lambda
        exec 'let g:StupidMapExpr_' . string(s:count) . ' = a:keys'

        " Bind function
        exec 'nnoremap <expr> ' . extra . a:trigger . ' g:StupidMapExpr_' . string(s:count) . '()'

        " Update counter
        let s:count += 1
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
call Map('n', '<leader>q', 'close')
call Map('n', '<leader>n', 'enew')
call Map('n', '<leader>s', 'wall')
call Map('n', '<leader><leader>', 'Files')
call Map('n', '<leader>f', 'Ranger')
call Map('n', '<leader>p', 'History')
call Map('n', '<leader>bk', 'bdelete!')

call Map('n', '<leader>g', 'Git')

call MapKeys('', '<leader>/', ':Rg ')
call MapKeys('', '<leader>:', 'q:')
call MapKeys('', '<leader>.', ':e %:p:h/')
call MapKeys('', 'D', '0d$')

for m in ['h', 'j', 'k', 'l']
    call MapKeys('', '<leader>' . m, '<C-w>' . m)
endfor
