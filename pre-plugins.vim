" Language Server Protocol --- {{{
let g:LanguageClient_serverCommands = {
            \ 'rust':   ['rls'],
            \ 'nix':    ['rnix-lsp'],
            \ 'python': ['pyls'],
            \ }
" }}}

" NCM2 completion --- {{{
augroup ncm2
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
function! s:align(name, regex, mapping)
    call Map('n', 'ga' . a:mapping, 'Tabularize ' . a:regex)
    exec 'vnoremap <silent> ga' . a:mapping . ' :Tabularize ' . a:regex . '<CR>'
endfunction

call s:align('equal',   '/=',                   '=')
call s:align('colon',   '/:\zs/l1l0',           ':')
call s:align('comment', '/\s\+\zs\(\/\/\\|#\)', '#')
call s:align('args',    '/,\s\zs/l1l0',         ',')

nnoremap ga/ :Tabularize /
vnoremap ga/ :Tabularize /

augroup tabular
    au!

    " Format markdown tables
    au FileType markdown inoremap <silent><buffer> \| \|<C-o>:TableFormat<CR>
    au FileType markdown nnoremap <silent><buffer> <C-c><C-c> :TableFormat<CR>
augroup END

" }}}
