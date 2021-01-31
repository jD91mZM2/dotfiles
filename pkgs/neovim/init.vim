let g:VimrcDir = expand('<sfile>:p:h')

function! Abspath(relative)
    return g:VimrcDir . '/' . a:relative
endfunction
function! Load(relative)
    exec 'source ' . Abspath(a:relative)
endfunction

let g:VimrcDirReal = expand('~/dotfiles/pkgs/neovim')

" Styling --- {{{
let g:airline_theme = 'dracula'
let g:airline_powerline_fonts = 1
" }}}

" Options --- {{{
set clipboard=unnamedplus
set expandtab
set hidden
set ignorecase
set mouse=a
set number
set relativenumber
set shiftwidth=0
set signcolumn=yes
set tabstop=4
" }}}

" Load files --- {{{
call Load('./utils.vim')

call Load('./keymap.vim')
call Load('./navigation.vim')
call Load('./terminal.vim')
call Load('./validation.vim')
" }}}

" Misc autocommands --- {{{
augroup s:general
    au!

    " Don't continue comment on 'o'
    au FileType * set formatoptions-=o

    " Use foldmethod=marker in vimrc
    au FileType vim,snippets setlocal foldmethod=marker
    au FileType vim,snippets normal! zR
augroup END
" }}}


" Highlight timer, stop highlighting after 3 seconds --- {{{
call Map('nvic', '<Plug>noh', 'noh')
function! NohReady(_id)
    au CursorMoved,CursorHold,CursorHoldI * ++once :call feedkeys("\<Plug>noh")
endfunction
function! NohTimer()
    if exists('s:nohtimerid')
        call timer_stop(s:nohtimerid)
    endif
    let s:nohtimerid = timer_start(3000, function('NohReady'))
endfunction
nnoremap <silent> n :call NohTimer()<CR>n
nnoremap <silent> * :call NohTimer()<CR>*
nnoremap <silent> # :call NohTimer()<CR>#
nnoremap <silent> N :call NohTimer()<CR>N
augroup s:nosearch
    au!
    au CmdlineLeave /,\? call NohTimer()
augroup END
" }}}

" Load all plugins --- {{{
call Load('./pre-plugins.vim')
packloadall
call Load('./post-plugins.vim')
" }}}

" Post-plugin styling --- {{{
function! s:highlight(group, extra, ...)
    let command = 'hi ' . a:group . a:extra
    for assign in a:000
        let list     = split(assign, '=')
        let color    = execute('echon g:dracula#palette.' . list[1] . '[1]')
        let command .= ' ' . list[0] . '=' . color
    endfor
    execute command
endfunction

colorscheme dracula
hi Normal ctermbg=NONE guibg=NONE

call s:highlight('CursorLine', ' cterm=NONE gui=NONE', 'guibg=bgdark', 'ctermbg=bgdark')
set cursorline
" }}}

" Highlight trailing spaces --- {{{
" See https://vim.fandom.com/wiki/Highlight_unwanted_spaces#Highlighting_with_the_match_command
hi ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+\%#\@<!$/

augroup s:extraws
    au!

    " Redraws the highlights when leaving insert mode
    au InsertLeave * redraw!
augroup END

call s:highlight('ExtraWhitespace', ' cterm=NONE gui=NONE', 'guibg=red', 'ctermbg=red')
" }}}
