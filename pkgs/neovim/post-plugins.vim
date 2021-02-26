" UltiSnips --- {{{
let g:UltiSnipsSnippetDirectories=[ (g:VimrcDirReal . '/snippets') ]
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

let s:skelMapping = {
            \   'flake.nix':      '_flake',
            \   '.gitlab-ci.yml': '_gitlab',
            \ }

function! s:skeleton(_id)
    let file = expand('%:t')
    let type = get(s:skelMapping, file, '')
    call feedkeys("\<C-\>\<C-n>i_skel" . type . "\<C-r>=UltiSnips#ExpandSnippet()\<CR>")
endfunction

augroup s:ultisnips
    au!

    " File Templates
    au BufNewFile *.* call timer_start(1, function('s:skeleton'))

    " Sometimes UltiSnips doesn't update
    au BufWrite *.snippets call UltiSnips#RefreshSnippets()
augroup END
" }}}

" Operators --- {{{
call operator#user#define_ex_command('sort', 'sort')
map gs <Plug>(operator-sort)

" Tabularize --- {{{
function! s:align(name, mapping, regex)
    call operator#user#define_ex_command(a:name, 'Tabularize ' . a:regex)
    exec 'map ga' . a:mapping . ' <Plug>(operator-' . a:name . ')'
    call Map('n', 'gaa' . a:mapping, 'Tabularize ' . a:regex)
endfunction

call s:align('equal',   '=', '/^[^=]\{-}\zs[.&+\-]*=/l1r1')
call s:align('field',   ':', '/:\zs/l1l0')
call s:align('comment', '#', '/\s\+\zs\(\/\/\\|#\)')
call s:align('args',    ',', '/,\s\zs/l1l0')
call s:align('closing', ']', '/[]]/l1l0')

nnoremap ga/ :Tabularize /
vnoremap ga/ :Tabularize /
" }}}
" }}}

" Firenvim --- {{{
let g:firenvim_config = {
            \   'localSettings': {
            \     '.*': {
            \       'takeover': 'never',
            \     },
            \   },
            \ }
" }}}
