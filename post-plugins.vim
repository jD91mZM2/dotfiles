" UltiSnips --- {{{
let g:UltiSnipsSnippetDirectories=[ (g:VimrcDirReal . '/snippets') ]
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'

function! s:skeleton(_id)
    call feedkeys("i_skel\<C-r>=UltiSnips#ExpandSnippet()\<CR>")
endfunction

augroup s:ultisnips
    au!

    " File Templates
    au BufNewFile * call timer_start(1, function('s:skeleton'))

    " Sometimes UltiSnips doesn't update
    au BufWrite * call UltiSnips#RefreshSnippets()
augroup END
" }}}

" Operators --- {{{
call operator#user#define_ex_command('sort', 'sort')
map gs <Plug>(operator-sort)
" }}}
