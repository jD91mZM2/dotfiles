# BASED ON grml-zsh-config because it had all the things I wanted by default.

# --------------------------------
# General
# --------------------------------

eval "$(dircolors ~/.dircolors)"

alias ls="ls -CF --color=auto"
alias clear='command clear; echo -ne "\e[3J"'
alias tmux="tmux -2"
alias git=hub

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

# https://github.com/pstadler/keybase-gpg-github/issues/11
export GPG_TTY="$(tty)"

# --------------------------------
# Options
# --------------------------------

setopt HIST_IGNORE_DUPS

# --------------------------------
# Plugins
# --------------------------------

source /usr/share/z/z.sh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
