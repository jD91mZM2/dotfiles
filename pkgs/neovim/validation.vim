" NCM2 completion --- {{{
set completeopt=noinsert,menuone,noselect

augroup s:ncm2
    au!

    au BufEnter * call ncm2#enable_for_buffer()

    au User Ncm2Plugin call ncm2#register_source({
                \   'name' : 'html',
                \   'priority': 9,
                \   'scope': ['html'],
                \   'mark': 'html',
                \   'word_pattern': '[\w\-]+',
                \   'complete_pattern': '<',
                \   'on_complete': ['ncm2#on_complete#omni', 'htmlcomplete#CompleteTags'],
                \ })
augroup END
" }}}

" ALE --- {{{
" I use LanguageClient-neovim for its ncm2 support
let g:ale_disable_lsp = 1

" Fix on save
let g:ale_fix_on_save = 1
let g:ale_fixers = {
            \   '*':      ['remove_trailing_lines', 'trim_whitespace'],
            \   'go':     ['goimports'],
            \   'json':   ['jq'],
            \   'nix':    ['nixpkgs-fmt'],
            \   'python': ['black'],
            \   'rust':   ['rustfmt'],
            \ }

let g:ale_rust_rustfmt_options = '+nightly'

" Toggle whether to fix on save
call Map('n', '<C-f>', 'let g:ale_fix_on_save = !g:ale_fix_on_save')

" Jump to errors
call Map('n', '<M-n>', 'ALENextWrap')
call Map('n', '<M-p>', 'ALEPreviousWrap')
" }}}

" Language Server Protocol --- {{{
" Configure language servers
let g:LanguageClient_serverCommands = {
            \   'go':     ['gopls'],
            \   'nix':    ['rnix-lsp'],
            \   'python': ['pyls'],
            \   'rust':   ['rls'],
            \ }

" Send settings via lsp command `didChangeConfiguration`
let g:LanguageClient_settingsPath = [
            \   Abspath('./lsp_settings.json'),
            \   '.vim/settings.json',
            \ ]

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
let g:AutoPairsShortcutJump = '<Plug>(autopairs-jump)'
let g:AutoPairsShortcutToggle = '<Plug>(autopairs-toggle)'
" }}}
