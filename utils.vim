let s:count = 0

function! AnonFunc(func)
    " Create placeholder name
    let name = 'g:AnonFunc_' . string(s:count)
    let s:count += 1

    " Bind to a:func
    exec 'let ' . name . ' = a:func'

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
