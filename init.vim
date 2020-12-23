" Styling --- {{{
let g:airline_theme = 'dracula'
let g:airline_powerline_fonts = 1
" }}}

" Options --- {{{
set mouse=a
set ignorecase
set number
set relativenumber
set clipboard=unnamedplus
set expandtab
set tabstop=4
set shiftwidth=0
set hidden
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
call s:map('n', 'gt', 'Buffers')

call s:map('n', '<leader>H', 'split')
call s:map('n', '<leader>V', 'vsplit')
call s:map('n', '<leader>%', 'source %')
call s:map('n', '<leader>1', 'only')
call s:map('n', '<leader>q', 'q')
call s:map('n', '<leader><leader>', 'Files')
call s:map('n', '<leader>f', 'Ranger')

call s:map('n', '<leader>g', 'Git')

function! ToggleNERD()
    if g:NERDTree.IsOpen()
        NERDTreeClose
    else
        NERDTreeFind
    endif
endfunction

call s:map('n', '<leader>t', 'call ToggleNERD()')

exec 'nnoremap <leader>/ :Rg '
nnoremap <leader>: q:
nnoremap <leader>h <C-w>h
nnoremap <leader>j <C-w>j
nnoremap <leader>k <C-w>k
nnoremap <leader>l <C-w>l
nnoremap D 0d$

" }}}

" Misc autocommands --- {{{
augroup general
    au!
    au TermClose * bdelete!
augroup END
" }}}


" Don't highlight search --- {{{
call s:map('nvic', '<Plug>noh', 'noh')
function! NohTimer()
    if exists("s:nohtimerid")
        call timer_stop(s:nohtimerid)
    endif
    let s:nohtimerid = timer_start(3000, { _id -> feedkeys("\<Plug>noh") })
endfunction
nnoremap <silent> n :call NohTimer()<CR>n
nnoremap <silent> N :call NohTimer()<CR>N
augroup nosearch
    au!
    au CmdlineLeave /,\? call NohTimer()
augroup END
" }}}

" Plugin configuration --- {{{

" Language Server Protocol --- {{{
let g:LanguageClient_serverCommands = {
            \ 'rust': ['rls'],
            \ 'nix': ['rnix-lsp'],
            \ }
" }}}

" NCM2 completion --- {{{
augroup ncm2
    au BufEnter * call ncm2#enable_for_buffer()
augroup END

set completeopt=noinsert,menuone,noselect
" --- }}}

" FZF --- {{{
let $FZF_DEFAULT_COMMAND = 'fd'
" }}}

" }}}

" Load all plugins
packloadall

" Post-plugin styling --- {{{
colorscheme dracula
hi Normal ctermbg=NONE guibg=NONE

let s:clcolor = g:dracula#palette.bgdark[1]
execute 'hi CursorLine cterm=NONE gui=NONE guibg=' . s:clcolor . ' ctermbg=' . s:clcolor
set cursorline
" }}}

" Operators --- {{{
call operator#user#define_ex_command('sort', 'sort')
map gs <Plug>(operator-sort)
" }}}
