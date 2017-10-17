[[ -s "$HOME/.profile" ]] && source "$HOME/.profile"

# Arch Linux <3

[ -z "$SSH_AUTH_SOCK" ] && [ -z "$SSH_AGENT_PID" ] && eval $(ssh-agent)
[ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ] && exec startx -keeptty
