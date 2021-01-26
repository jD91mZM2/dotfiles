" quickfix-reflector.vim --- {{{
let g:qf_join_changes = 1
" }}}

" Configure fzf for navigation --- {{{
let $FZF_DEFAULT_COMMAND = 'fd -H -E .git'
let $FZF_DEFAULT_OPTS    = '--bind alt-q:select-all+accept'

call Map('n',     'gt',        'Buffers')
call Map('n',     '<leader>p', 'History')
call MapKeys('n', '<leader>/', ':Rg ')
" }}}

" Configure NerdTREE --- {{{
function! ToggleNERD()
    if g:NERDTree.IsOpen()
        NERDTreeClose
    else
        let path = expand('%:p')

        " Open current directory (rooter makes this the project root)
        NERDTreeCWD

        " Select the current file
        exec 'NERDTreeFind ' . path
    endif
endfunction

let g:NERDTreeQuitOnOpen = 1
call Map('n', '<leader>t', 'call ToggleNERD()')
" }}}
