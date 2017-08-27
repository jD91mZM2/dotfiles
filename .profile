# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# Variables
PATH="$HOME/.rvm/rubies/default/bin:$PATH"
PATH="$PATH:$HOME/bin:$HOME/.local/bin"
PATH="$PATH:$HOME/.cargo/bin"
PATH="$PATH:$HOME/Coding/Go/bin"
PATH="$PATH:/usr/local/go/bin"
PATH="$PATH:/usr/local/kotlin/bin"
PATH="$PATH:$HOME/.llvm/build/bin"
export PATH

export GOPATH="$HOME/Coding/Go"
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src/"
export SHELL="/usr/bin/fish"
export VISUAL=nvim
export EDITOR="$VISUAL"

# https://github.com/electron/electron/issues/8455
export ELECTRON_FORCE_WINDOW_MENU_BAR=1
