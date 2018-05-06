# BASED ON grml-zsh-config because it had all the things I wanted by default.

# --------------------------------
# General
# --------------------------------

eval "$(dircolors ~/.dircolors)"

alias clear='clear; echo -ne "\e[3J"'
alias git=hub
alias ls="ls -CF --color=auto"
alias nix-shell="nix-shell --run zsh"
alias rsynca='rsync -avzhP'

unicopy() {
    [ -z "$1" ] && echo "unicopy <character>" && return
    unicode "$1" --format "{pchar}" | xclip -sel clip
}
loop() {
    if [ -z "$1" ] || [ -z "$2" ]; then
        echo "loop <n> <command...>"
        return
    fi

    for _ in $(seq 1 "$1"); do
        eval "${@:2}"
    done
}
powerline() {
    PS1="$(powerline-rs --shell zsh $?)"
}
precmd_functions+=(powerline)

# --------------------------------
# Options
# --------------------------------

setopt HIST_IGNORE_DUPS

# --------------------------------
# Plugins
# --------------------------------

# Plugins are loaded using NixOS's things.
# I'm using:
# - autojump
# - grml-zsh-config
# - zsh-autosuggestions
# - zsh-completions
# - zsh-syntax-highlighting

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
