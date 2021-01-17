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
call Map('n',     'gt',        'Buffers')
call Map('n',     '<leader>p', 'History')
call MapKeys('n', '<leader>/', ':Rg ')
" }}}

" NERDTree --- {{{
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

" Tabularize --- {{{
function! s:align(regex, mapping)
    call Map('n', 'ga' . a:mapping, 'Tabularize ' . a:regex)
    exec 'vnoremap <silent> ga' . a:mapping . ' :Tabularize ' . a:regex . '<CR>'
endfunction

call s:align('/^[^=]\{-}\zs[.&+\-]*=/l1r1', '=')
call s:align('/:\zs/l1l0',                  ':')
call s:align('/\s\+\zs\(\/\/\\|#\)',        '#')
call s:align('/,\s\zs/l1l0',                ',')
call s:align('/[]]/l1l0',                   ']')

nnoremap ga/ :Tabularize /
vnoremap ga/ :Tabularize /
" }}}

" Fugitive --- {{{
call Map('n', '<leader>g', 'Git')

function! s:fugitiveMain()
    call MapKeys('nb', ':', ':G ')

    call MapKeys('nb', 'pp', ':G push')
    call MapKeys('nb', 'pu', { -> ':G push -u  ' . FugitiveHead() . "\<C-Left>\<Left>" })
    call MapKeys('nb', 'pf', ':G push --force-with-lease')

    call MapKeys('nb', 'bs', ':G switch -c ')
    call MapKeys('nb', 'bb', ':G switch ')
    call MapKeys('nb', 'bx', ':G branch -D ')
    call MapKeys('nb', 'bX', ":G push  :<Left><Left>")

    call MapKeys('nb', 'mm', ':G merge ')
    call MapKeys('nb', 'ms', ':G merge --squash ')

    call MapKeys('nb', 'MM', ':G remote add ')

    call MapKeys('nb', 'OO', ':G reset --hard HEAD^')
    call MapKeys('nb', 'Os', ':G reset --soft HEAD^ ')

    call Map('nb', 'F',  'G pull')
    call Map('nb', 'll', 'close \| vert G log')
    call Map('nb', 'lr', 'close \| vert G reflog')
    call Map('nb', 'q',  'close')
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

" Neoformat --- {{{
augroup s:fmt
    au!

    " Run formatters on save
    au BufWritePre *.nix Neoformat
augroup END
" }}}

" vim-table-mode  --- {{{
" Always look for '|' on new lines
let g:table_mode_always_active = 1

" Disable table mode mappings
let g:table_mode_map_prefix = '<Plug>table-mode'
" }}}

" NeoVim Treesitter --- {{{
lua <<EOF
require'nvim-treesitter.configs'.setup {
    ensure_installed = "maintained",
    highlight = {
        enable = true,
    },
    indent = {
        enable = true,
        disable = { 'python' },
    },
    incremental_selection = {
        enable = true,
        keymaps = {
            init_selection = "<C-e>",
            node_incremental = "<C-e>",
        },
    },
}
EOF
" }}}
