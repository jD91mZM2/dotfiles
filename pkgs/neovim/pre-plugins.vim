" NCM2 completion --- {{{
augroup s:ncm2
    au BufEnter * call ncm2#enable_for_buffer()
augroup END

set completeopt=noinsert,menuone,noselect
" }}}

" ALE --- {{{
" I use LanguageClient-neovim for its ncm2 support
let g:ale_disable_lsp = 1

" Fix on save
let g:ale_fix_on_save = 1
let g:ale_fixers = {
            \ '*':      ['remove_trailing_lines', 'trim_whitespace'],
            \ 'go':     ['goimports'],
            \ 'nix':    ['nixpkgs-fmt'],
            \ 'python': ['black'],
            \ 'rust':   ['rustfmt'],
            \ }
let g:ale_rust_rustfmt_options = '+nightly'

" Toggle whether to fix on save
call Map('n', '<C-f>', 'let g:ale_fix_on_save = !g:ale_fix_on_save')

" Jump to errors
call Map('n', '<M-n>', 'ALENextWrap')
call Map('n', '<M-p>', 'ALEPreviousWrap')
" }}}

" Language Server Protocol --- {{{
let g:LanguageClient_serverCommands = {
            \ 'go':     ['go-langserver', '-gocodecompletion'],
            \ 'nix':    ['rnix-lsp'],
            \ 'python': ['pyls'],
            \ 'rust':   ['rls'],
            \ }

call Map('n', '<leader>ld', 'call LanguageClient#textDocument_definition()')
call Map('n', '<leader>lr', 'call LanguageClient#textDocument_rename()')
call Map('n', '<leader>lf', 'call LanguageClient#textDocument_formatting()')
call Map('n', '<leader>lt', 'call LanguageClient#textDocument_typeDefinition()')
call Map('n', '<leader>lx', 'call LanguageClient#textDocument_references()')
call Map('n', '<leader>la', 'call LanguageClient_workspace_applyEdit()')
call Map('n', '<leader>lc', 'call LanguageClient#textDocument_completion()')
call Map('n', '<leader>lh', 'call LanguageClient#textDocument_hover()')
call Map('n', '<leader>ls', 'call LanguageClient_textDocument_documentSymbol()')
call Map('n', '<leader>lm', 'call LanguageClient_contextMenu()')
" }}}

" Autopairs --- {{{
" Free up M-n and M-p for ALE
let g:AutoPairsShortcutJump = "<Plug>(autopairs-jump)"
let g:AutoPairsShortcutToggle = "<Plug>(autopairs-toggle)"
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
