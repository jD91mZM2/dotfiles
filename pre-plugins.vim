" Language Server Protocol --- {{{
let g:LanguageClient_serverCommands = {
            \ 'rust':   ['rls'],
            \ 'nix':    ['rnix-lsp'],
            \ 'python': ['pyls'],
            \ }
" }}}

" NCM2 completion --- {{{
augroup s:ncm2
    au BufEnter * call ncm2#enable_for_buffer()
augroup END

set completeopt=noinsert,menuone,noselect
" }}}

" FZF --- {{{
let $FZF_DEFAULT_COMMAND = 'fd -H -E .git'
" }}}

" NERDTree --- {{{
function! ToggleNERD()
    if g:NERDTree.IsOpen()
        NERDTreeClose
    else
        NERDTreeFind
    endif
endfunction

let g:NERDTreeQuitOnOpen = 1
call Map('n', '<leader>t', 'call ToggleNERD()')
" }}}

" Tabularize --- {{{
function! s:align(regex, mapping)
    call Map('n', 'ga' . a:mapping, 'Tabularize ' . a:regex)
    exec 'vnoremap <silent> ga' . a:mapping . ' :Tabularize ' . a:regex . '<CR>'
endfunction

call s:align('/^[^=]\{-}\zs[.&+\-]*=/l1r1', '=')
call s:align('/:\zs/l1l0',                  ':')
call s:align('/\s\+\zs\(\/\/\\|#\)',        '#')
call s:align('/,\s\zs/l1l0',                ',')

nnoremap ga/ :Tabularize /
vnoremap ga/ :Tabularize /

augroup s:tabular
    au!

    " Format markdown tables
    au FileType markdown inoremap <silent> <buffer> \| \|<C-o>:TableFormat<CR><C-o>f\|<Right>
    au FileType markdown nnoremap <silent> <buffer> <C-c><C-c> :TableFormat<CR>
augroup END
" }}}

" Fugitive --- {{{
function! s:fugitiveMain()
    call MapKeys('b', 'pp', ':G push')
    call MapKeys('b', 'pf', ':G push --force-with-lease')
    call MapKeys('b', 'b', ':G switch -c ')
    call MapKeys('b', 'mm', ':G remote add ')
    call MapKeys('b', 'mu', ':G branch --set-upstream-to ')
    call Map('nb', 'll', 'close \| vert G log')
    call Map('nb', 'lr', 'close \| vert G reflog')
    call Map('nb', 'q', 'close')
endfunction
function! s:fugitiveGit()
    call Map('nb', 'q', 'close \| G')
endfunction
function! s:fugitiveCommit()
    call Map('nb', '<C-c><C-c>', 'wq')
endfunction
augroup s:fugitive
    au!

    " Binds additional stuff
    au FileType fugitive  call s:fugitiveMain()
    au FileType git       call s:fugitiveGit()
    au FileType gitcommit call s:fugitiveCommit()
augroup END
" }}}
