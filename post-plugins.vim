" UltiSnips --- {{{
let g:UltiSnipsSnippetDirectories=[ (g:VimrcDirReal . '/snippets') ]
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

let s:skelMapping = {
            \ 'flake.nix':      '_flake',
            \ '.gitlab-ci.yml': '_gitlab',
            \ }

function! s:skeleton(_id)
    let file = expand('%:t')
    let type = get(s:skelMapping, file, '')
    call feedkeys("\<Esc>i_skel" . type . "\<C-r>=UltiSnips#ExpandSnippet()\<CR>")
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
" }}}
