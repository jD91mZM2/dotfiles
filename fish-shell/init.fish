if ps -o comm | grep nvim > /dev/null
	exit
end

# Aliases
alias rm=rmtrash
alias rmdir=rmdirtrash
alias reset="command reset; insult | cowsay"

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

# Default Ruby
rvm default

# Dircolors
bass eval `dircolors ~/.dir_colors/dircolors`

# INSULTS!
insult | cowsay
