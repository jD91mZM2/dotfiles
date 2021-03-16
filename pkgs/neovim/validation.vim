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
            \   '*':       ['remove_trailing_lines', 'trim_whitespace'],
            \   'go':      ['goimports'],
            \   'haskell': ['ormolu'],
            \   'json':    ['jq'],
            \   'nix':     ['nixpkgs-fmt'],
            \   'python':  ['black'],
            \   'rust':    ['rustfmt'],
            \ }

" Disable linters that break or bug out for me
let g:ale_linters_ignore = {
            \  'nix': ['nix'],
            \ }

let g:ale_rust_rustfmt_options = '+nightly'

" Toggle whether to fix on save
call Map('n', '<C-f>', 'let g:ale_fix_on_save = !g:ale_fix_on_save')

" Jump to errors
call Map('n', '<M-n>', 'ALENextWrap')
call Map('n', '<M-p>', 'ALEPreviousWrap')
" }}}

" Echodoc --- {{{
let g:echodoc#enable_at_startup = 1
let g:echodoc#type = 'floating'
" }}}

" Language Server Protocol --- {{{
" Configure language servers
let g:LanguageClient_serverCommands = {
            \   'go':      ['gopls'],
            \   'haskell': ['haskell-language-server'],
            \   'nix':     ['rnix-lsp'],
            \   'python':  ['pyls'],
            \   'rust':    ['rls'],
            \   'tex':     ['texlab'],
            \ }

" Send settings via lsp command `didChangeConfiguration`
let g:LanguageClient_settingsPath = [
            \   Abspath('./lsp_settings.json'),
            \   '.vim/settings.json',
            \ ]

let g:LanguageClient_loggingFile = tempname()
let g:LanguageClient_loggingLevel = 'DEBUG'

call MapLeader('n', 'l d', 'call LanguageClient#textDocument_definition()')
call MapLeader('n', 'l r', 'call LanguageClient#textDocument_rename()')
call MapLeader('n', 'l f', 'call LanguageClient#textDocument_formatting()')
call MapLeader('n', 'l t', 'call LanguageClient#textDocument_typeDefinition()')
call MapLeader('n', 'l x', 'call LanguageClient#textDocument_references()')
call MapLeader('n', 'l a', 'call LanguageClient_workspace_applyEdit()')
call MapLeader('n', 'l c', 'call LanguageClient#textDocument_completion()')
call MapLeader('n', 'l h', 'call LanguageClient#textDocument_hover()')
call MapLeader('n', 'l s', 'call LanguageClient_textDocument_documentSymbol()')
call MapLeader('n', 'l m', 'call LanguageClient_contextMenu()')

call CreateIfNotExists('g:which_key_map', {})
let g:which_key_map['l'] = {
            \   'd': 'Goto Definition',
            \   'r': 'Rename',
            \   'f': 'Format',
            \   't': 'Type Definition',
            \   'x': 'References',
            \   'a': 'Apply Suggestion',
            \   'c': 'Completion',
            \   'h': 'Hover',
            \   's': 'Document Symbol',
            \   'm': 'Context Menu',
            \ }
" }}}

" Autopairs --- {{{
" Free up M-n and M-p for ALE
let g:AutoPairsShortcutJump = '<Plug>(autopairs-jump)'
let g:AutoPairsShortcutToggle = '<Plug>(autopairs-toggle)'
" }}}
