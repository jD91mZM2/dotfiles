function fish_greeting
end

# stderred
bass export LD_PRELOAD="$HOME/.stderred/stderred/build/libstderred.so\${LD_PRELOAD:+:$LD_PRELOAD}"
# Dircolors
bass eval `dircolors ~/.dir_colors/dircolors`
