" UltiSnips --- {{{
let g:UltiSnipsSnippetDirectories=[ expand('~/dotfiles/nix-exprs/pkgs/neovim/snippets') ]
let g:UltiSnipsExpandTrigger='<tab>'
let g:UltiSnipsJumpForwardTrigger='<tab>'
let g:UltiSnipsJumpBackwardTrigger='<s-tab>'
" }}}

" Operators --- {{{
call operator#user#define_ex_command('sort', 'sort')
map gs <Plug>(operator-sort)
" }}}
