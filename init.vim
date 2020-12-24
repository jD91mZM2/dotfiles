let s:dir = expand('<sfile>:p:h')

function! s:load(relative)
    let absolute = s:dir . '/' . a:relative
    exec 'source ' . absolute
endfunction

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
set formatoptions-=o
" }}}

" Keyboard shortcuts --- {{{
call s:load('./keymap.vim')
" }}}

" Misc autocommands --- {{{
augroup general
    au!
    au TermClose * bdelete!
augroup END
" }}}


" Don't highlight search --- {{{
call Map('nvic', '<Plug>noh', 'noh')
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
call s:load('./plugins.vim')
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
