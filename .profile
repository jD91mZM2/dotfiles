# $PATH
PATH="$PATH:$(ruby -e "print Gem.user_dir")/bin"
PATH="$PATH:$HOME/.cargo/bin"
PATH="$PATH:$HOME/.local/bin"
PATH="$PATH:$HOME/Coding/Go/bin"
PATH="$PATH:$HOME/bin:$HOME/.local/bin"

export PATH

# Variables
export EDITOR=nvim
export GOPATH="$HOME/Coding/Go"
export QT_QPA_PLATFORMTHEME=gtk2
export RUST_BACKTRACE=1
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
export TERMINAL=konsole
export VISUAL=nvim

# Use GNOME Keyring for ssh
eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

# https://github.com/electron/electron/issues/8455
export ELECTRON_FORCE_WINDOW_MENU_BAR=1

# https://bbs.archlinux.org/viewtopic.php?id=159016
export _JAVA_AWT_WM_NONREPARENTING=1

# `less` colors
export LESS_TERMCAP_md=$'\e[01;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;44;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[01;32m'
