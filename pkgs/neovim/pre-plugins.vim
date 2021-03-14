" Fugitive --- {{{
call MapLeader('n', 'g', 'Git')

function! g:CmdGDelBranch(remote, branch)
    exec 'G branch -D ' . a:branch
    exec 'G push ' . a:remote ' :' . a:branch
endfunction
command! -nargs=* GDelBranch call CmdGDelBranch(<f-args>)

function! s:fugitiveMain()
    call MapKeys('nb', ':', ':G ')

    call MapKeys('nb', 'pp', ':G push')
    call MapKeys('nb', 'pu', { -> ':G push -u  ' . FugitiveHead() . "\<C-Left>\<Left>" })
    call MapKeys('nb', 'pf', ':G push --force-with-lease')

    call MapKeys('nb', 'bs', ':G switch -c ')
    call MapKeys('nb', 'bb', ':G switch ')
    call MapKeys('nb', 'bx', ':GDelBranch ')

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

" vim-signify --- {{{
set updatetime=100
" }}}

" vim-table-mode --- {{{
" Always look for '|' on new lines
let g:table_mode_always_active = 1

" Disable table mode mappings
let g:table_mode_map_prefix = '<Plug>table-mode'
" }}}

" Vimtex --- {{{
let g:vimtex_view_method = 'zathura'

augroup s:vimtex
    au!

    " Bind C-c
    au FileType tex map <C-c> <localleader>l
augroup END
" }}}

" NeoVim Treesitter --- {{{
lua <<EOF
require('nvim-treesitter.configs').setup {
    ensure_installed = "maintained",
    highlight = {
        enable = true,
        disable = { 'nix' },
    },
    indent = {
        enable = true,
        disable = { 'python', 'rust' },
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
