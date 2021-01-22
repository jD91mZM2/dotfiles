let mapleader = ' '

let s:modes = ['n', 'v', 'i', 'c', 't']

function! s:rawMap(mode, extra, trigger, prefix, keys, suffix)
    let extra = a:extra

    if IsString(a:keys)
        let bindTo = a:prefix . a:keys . a:suffix
    elseif IsFunction(a:keys)
        let name = AnonFunc({ -> a:prefix . a:keys() . a:suffix })

        let extra  .= '<expr> '
        let bindTo  = name . '()'
    endif

    exec a:mode . 'noremap ' . extra . a:trigger . ' ' . bindTo
endfunction

function! Map(opts, trigger, command)
    let opts = Opts(a:opts)
    let extra = Maybe(opts.Has('b'), '<buffer> ')

    let wrapper = {
                \ 'n': [ ':',             "\<CR>"        ],
                \ 'v': [ "\<C-c>:",       "\<CR>gv"      ],
                \ 'i': [ "\<C-o>:",       "\<CR>"        ],
                \ 'c': [ "\<C-c>:",       "\<CR>:\<C-p>" ],
                \ 't': [ "\<C-\>\<C-n>:", "\<CR>i"       ],
                \ }

    for m in opts.Intersecting(s:modes)
        let silent = Maybe(m !=# 'c', '<silent> ')

        let prefix = wrapper[m][0]
        let suffix = wrapper[m][1]

        call s:rawMap(m, silent . extra, a:trigger, prefix, a:command, suffix)
    endfor
endfunction

function! MapKeys(opts, trigger, keys)
    let opts = Opts(a:opts)
    let extra = Maybe(opts.Has('b'), '<buffer> ')

    for m in opts.Intersecting(s:modes)
        call s:rawMap(m, extra, a:trigger, "\<C-\>\<C-n>", a:keys, '')
    endfor
endfunction

call Map('nvic', '<Left>',  'echo "You must never use arrow keys!"')
call Map('nvic', '<Right>', 'echo "You must never use arrow keys!"')
call Map('nvic', '<Up>',    'echo "You must never use arrow keys!"')
call Map('nvic', '<Down>',  'echo "You must never use arrow keys!"')

call Map('n', '<C-s>', 'w')

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
cnoremap <M-h> <C-Left>
cnoremap <M-l> <C-Right>
