bass eval `dircolors ~/.dircolors`

alias clear='command clear; echo -ne "\e[3J"'
alias tmux='tmux -2'
alias git=hub

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

# Arch Linux <3

test -z "$SSH_AUTH_SOCK" -a -z "$SSH_AUTH_PID"; bass eval (ssh-agent)"true"
# Because ssh-agent ends with a semicolon and that messes up bass for some reason.
if begin status --is-login; and test -z "$DISPLAY" -a -n "$XDG_VTNR"; and test "$XDG_VTNR" = 1; end
    bass source ~/.profile
    exec startx -keeptty
end
