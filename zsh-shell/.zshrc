eval "$(dircolors ~/.dircolors)"

alias ls='ls -CF --color=auto'
alias clear='command clear; echo -ne "\e[3J"'
alias tmux='tmux -2'
alias git=hub

rm() {
    if echo "$1" | grep "^/tmp/" > /dev/null; then
        # Special case for RVM.
        command rm "$@"
    else
        echo "Don't use rm, use \`trash\`."
    fi
}
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

# https://unix.stackexchange.com/a/140499
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word

# ---------------------------------------------
# Plugins
# ---------------------------------------------

source ~/.zsh_plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/.zsh_plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# ---------------------------------------------
# Boring stuff
# ---------------------------------------------

# added by travis gem
[ -f /home/jD91mZM2/.travis/travis.sh ] && source /home/jD91mZM2/.travis/travis.sh

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
