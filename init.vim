" Styling
let g:airline_theme = 'dracula'
let g:airline_powerline_fonts = 1

" Options --- {{{
set mouse=a
set ignorecase
set number
set relativenumber
set clipboard=unnamedplus
set expandtab
set tabstop=4
set shiftwidth=0
" }}}

" Keyboard shortcuts --- {{{
let mapleader = ' '

function! s:map(mode, key, command)
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
        execute 'cnoremap <silent> ' . a:key . ' <C-c>:' . a:command . '<CR>:<C-p>'
    endif
endfunction

call s:map('nvi', '<Left>', 'echo "You must never use arrow keys!"')
call s:map('nvi', '<Right>', 'echo "You must never use arrow keys!"')
call s:map('nvic', '<Up>', 'echo "You must never use arrow keys!"')
call s:map('nvic', '<Down>', 'echo "You must never use arrow keys!"')

call s:map('n', '<C-S>', 'w')
call s:map('n', '<leader>H', 'split')
call s:map('n', '<leader>V', 'vsplit')
call s:map('n', '<leader>%', 'source %')
call s:map('n', '<leader>1', 'only')
call s:map('n', '<leader>q', 'q')
call s:map('n', '<leader><leader>', 'Ranger')
call s:map('n', '<leader>t', 'NERDTreeToggle')

nnoremap D 0d$
nnoremap <leader>h <C-w>h
nnoremap <leader>l <C-w>l
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>: q:

" }}}

" Misc autocommands
augroup general
    au!
    au TermClose * :bdelete!
augroup END

" Don't highlight search --- {{{
call s:map('nvic', '<F1>noh', 'noh')
function! NohTimer()
    if exists("s:nohtimerid")
        call timer_stop(s:nohtimerid)
    endif
    let s:nohtimerid = timer_start(3000, { _id -> feedkeys("\<F1>noh") })
endfunction
nnoremap <silent> n :call NohTimer()<CR>n
nnoremap <silent> N :call NohTimer()<CR>N
augroup nosearch
    au!
    au CmdlineLeave /,\? :call NohTimer()
augroup END
" }}}

" Load all plugins
packloadall

" Post-plugin styling
colorscheme dracula
hi Normal ctermbg=NONE guibg=NONE

let s:clcolor = g:dracula#palette.bgdark[1]
execute 'hi CursorLine cterm=NONE gui=NONE guibg=' . s:clcolor . ' ctermbg=' . s:clcolor
set cursorline
