bass eval `dircolors ~/.dircolors`

alias clear='command clear; echo -ne "\e[3J"'
alias tmux='tmux -2'
alias git=hub

function rm --description "Use `trash`"
    if set -q argv[2]; and echo "$argv[2]" | grep "^/tmp/" > /dev/null
        # Special case for RVM.
        command rm $argv
    else
        echo "Don't use `rm`, use `trash`."
    end
end

function unicopy --description "unicopy <character>" --argument character
    if test -z $character
        echo "unicopy <character>"
        return
    end
    unicode $character --format "{pchar}" | xclip -sel clip
end

function loop --description "loop <n> <command>" --argument n command
    if begin test -z $n; or test -z $command; end
        echo "loop <n> <command>"
        return
    end

    for _ in (seq 1 $n)
        eval $argv[2..-1]
    end
end

function fish_prompt
    powerline-shell --shell bare $status
end

# https://github.com/pstadler/keybase-gpg-github/issues/11
set -gx GPG_TTY (tty)

# Because I use Arch now =)
if status --is-login
    if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
        test -s "$HOME/.profile"; and bass source "$HOME/.profile"
        exec startx -- -keeptty
    end
end
