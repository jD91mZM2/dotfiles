" Create and return a dummy name for a lambda or function reference
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

" Return true if variable is a string
function! IsString(var)
    return type(a:var) ==# type('hello')
endfunction

" Return true if variable is a function
function! IsFunction(var)
    return type(a:var) ==# type(function('IsFunction'))
endfunction

" Return true if variable is a list
function! IsList(var)
    return type(a:var) ==# type([])
endfunction

" Create the quoted variable if it doesn't exist
function! CreateIfNotExists(var, default)
    if !exists(a:var)
        exec 'let ' . a:var . ' = a:default'
    endif
endfunction

" Return a string representing the index of a dictionary
function! DictIndex(dict_path, string)
    return a:dict_path . "['" . a:string . "']"
endfunction

" Create dictionaries up to the given search path
" |  CreateDicts('g:which_key_map', [ 'a', 'b', 'c' ])
" => g:which_key_map['a']['b']['c']
function! CreateDicts(var, indicies)
    let search_path = a:var
    call CreateIfNotExists(search_path, {})

    for index in a:indicies
        let search_path = DictIndex(search_path, index)
        call CreateIfNotExists(search_path, {})
    endfor

    return search_path
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

" Create a shell script given the following lines. Each line can either be a
" shell string, or a list of arguments which will be quoted.
function! ShellScript(lines)
    return join(map(a:lines, function('s:shellLine')), ';')
endfunction

" Return string if condition is true or empty string if condition is false
function! Maybe(cond, str)
    return a:cond ? a:str : ''
endfunction

" Return an options object given the string of option characters
function! Opts(opts)
    let options = a:opts

    return {
                \   'Has': { opt -> options =~# opt },
                \   'Intersecting': { list -> filter(copy(list), { _i, opt -> options =~# opt }) },
                \ }
endfunction
