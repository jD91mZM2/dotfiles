eval "$(dircolors ~/.dircolors)"

alias ls='ls -CF --color=auto'
alias clear='command clear; echo -ne "\e[3J"'
alias tmux='tmux -2'
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
    PS1="$(powerline-shell --shell zsh $?)"
}
precmd_functions+=(powerline)

# https://github.com/pstadler/keybase-gpg-github/issues/11
export GPG_TTY="$(tty)"

# --------------------------------
# Options
# --------------------------------

export HISTFILE="$HOME/.zsh_history"
export SAVEHIST=100

setopt APPEND_HISTORY
setopt HIST_IGNORE_DUPS

# --------------------------------
# Keybinds
# --------------------------------

bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[3~"   delete-char
bindkey "^U"      backward-kill-line
bindkey "^Y"      yank

# --------------------------------
# Plugins
# --------------------------------

source /usr/share/zsh/plugins/zsh-autosuggestions
source /usr/share/zsh/plugins/zsh-completions
source /usr/share/zsh/plugins/zsh-syntax-highlighting

export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="bg=10"
