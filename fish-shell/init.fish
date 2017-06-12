# Aliases
alias rm="rmtrash"
alias rmdir="rmdirtrash"

function unicopy --description "unicopy <character>"
	if test -z $argv[1]
		echo "unicopy <character>"
		return
	end
	unicode $argv[1] --format "{pchar}" | xclip -sel clip
end

# Default Ruby
rvm default

# Dircolors
bass eval `dircolors ~/.dir_colors/dircolors`
