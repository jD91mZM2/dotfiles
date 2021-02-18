let s:count = 0

function! AnonFunc(func)
    " Create placeholder name
    let name = 'AnonFunc_' . string(s:count)
    let s:count += 1

    " Bind global variable to a:func
    let code = 'let g:' . name . '_Var = a:func' . "\n"

    " Use variable in global function
    let code .= 'function! ' . name . '(...)' . "\n"
    let code .= 'return g:' . name . '_Var(a:000)' . "\n"
    let code .= 'endfunction'
    exec code

    " Return
    return name
endfunction

function! IsString(var)
    return type(a:var) ==# type('hello')
endfunction

function! IsFunction(var)
    return type(a:var) ==# type(function('IsFunction'))
endfunction

function! IsList(var)
    return type(a:var) ==# type([])
endfunction

function! s:shellArg(_index, arg)
    if IsString(a:arg)
        return shellescape(a:arg)
    elseif IsList(a:arg)
        return shellescape(ShellScript(a:arg))
    endif
endfunction

function! s:shellLine(_index, line)
    if IsString(a:line)
        return a:line
    elseif IsList(a:line)
        return join(map(a:line, function('s:shellArg')), ' ')
    endif
endfunction

function! ShellScript(lines)
    return join(map(a:lines, function('s:shellLine')), ';')
endfunction

function! Maybe(cond, str)
    return a:cond ? a:str : ''
endfunction

function! Opts(opts)
    let options = a:opts

    return {
                \   'Has': { opt -> options =~# opt },
                \   'Intersecting': { list -> filter(copy(list), { _i, opt -> options =~# opt }) },
                \ }
endfunction
