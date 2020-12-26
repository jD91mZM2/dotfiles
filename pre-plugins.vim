" Language Server Protocol --- {{{
let g:LanguageClient_serverCommands = {
            \ 'rust': ['rls'],
            \ 'nix': ['rnix-lsp'],
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
