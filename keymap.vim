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
    if a:opts =~# 't'
        execute 'tnoremap ' . a:trigger . ' <C-\><C-n>:' . a:command . '<CR>i'
    endif
endfunction

let s:count = 0
function! MapKeys(opts, trigger, keys)
    let extra = ''
    if a:opts =~# 'b'
        let extra .= '<buffer> '
    endif

    let modes = ['n', 'v', 'i', 'c', 't']

    if type(a:keys) == type("hello")
        for m in modes
            if a:opts =~# m
                exec m . 'noremap ' . extra . a:trigger . ' <C-\><C-n>' . a:keys
            endif
        endfor
    elseif type(a:keys) == type(function('MapKeys'))
        let l:Lambda = { -> "\<C-\>\<C-n>" . a:keys() }

        " Assign stupid unique global name to lambda
        exec 'let g:StupidMapExpr_' . string(s:count) . ' = l:Lambda'

        " Bind function
        for m in modes
            if a:opts =~# m
                exec m . 'noremap <expr> ' . extra . a:trigger . ' g:StupidMapExpr_' . string(s:count) . '()'
            endif
        endfor

        " Update counter
        let s:count += 1
    endif
endfunction

call Map('nvi',  '<Left>',  'echo "You must never use arrow keys!"')
call Map('nvi',  '<Right>', 'echo "You must never use arrow keys!"')
call Map('nvic', '<Up>',    'echo "You must never use arrow keys!"')
call Map('nvic', '<Down>',  'echo "You must never use arrow keys!"')

call Map('n', '<C-s>', 'w')
call Map('n', '<C-w>', '%s/\s\+$//')

call Map('n', '<leader>H',        'split')
call Map('n', '<leader>V',        'vsplit')
call Map('n', '<leader>1',        'only')
call Map('n', '<leader>q',        'close')
call Map('n', '<leader>n',        'enew')
call Map('n', '<leader>s',        'wall')
call Map('n', '<leader><leader>', 'Files')
call Map('n', '<leader>f',        'Ranger')
call Map('n', '<leader>bk',       'bdelete!')
call Map('n', '<leader>%',        'source ' . g:VimrcDirReal . '/init.vim')
call Map('n', '<leader>o',        'silent! !tmux new-window -c %:p:h')

call MapKeys('n', '<leader>:', 'q:')
call MapKeys('n', '<leader>.', ':e %:p:h/')
call MapKeys('n', 'D',         '0d$')

" Move left/right easier
cnoremap <C-h> <Left>
cnoremap <C-l> <Right>
cnoremap <A-h> <C-Left>
cnoremap <A-l> <C-Right>
