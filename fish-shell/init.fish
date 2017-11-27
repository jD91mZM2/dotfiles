if status --is-login
    bass source ~/.profile
end

bass eval `dircolors ~/.dircolors`

alias clear='command clear; echo -ne "\e[3J"'
alias tmux="tmux -2"
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
    powerline-go --shell bare --error $status --modules "venv,ssh,cwd,perms,git,hg,jobs,exit,root"
end

# https://github.com/pstadler/keybase-gpg-github/issues/11
set -gx GPG_TTY (tty)
