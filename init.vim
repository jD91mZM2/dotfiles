let g:VimrcDir = expand('<sfile>:p:h')

function! s:load(relative)
    let absolute = g:VimrcDir . '/' . a:relative
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
" }}}

" Keyboard shortcuts --- {{{
call s:load('./keymap.vim')
" }}}

" Misc autocommands --- {{{
augroup general
    au!

    " Don't show "Terminal Closed" screen
    au TermClose * bdelete!

    " Don't continue comment on 'o'
    au FileType * set formatoptions-=o

    " Use foldmethod=marker in vimrc
    au FileType vim setlocal foldmethod=marker
    au FileType vim normal! zR
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

" Load all plugins --- {{{
call s:load('./pre-plugins.vim')
packloadall
call s:load('./post-plugins.vim')
" }}}

" Post-plugin styling --- {{{
colorscheme dracula
hi Normal ctermbg=NONE guibg=NONE

let s:clcolor = g:dracula#palette.bgdark[1]
execute 'hi CursorLine cterm=NONE gui=NONE guibg=' . s:clcolor . ' ctermbg=' . s:clcolor
set cursorline
" }}}
