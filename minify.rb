input = STDIN.read

output = ""
string = false
escapes = 0
input.split("").each do |c|
	if c == '"' && escapes % 2 == 0
		string = !string
		escapes = 0
	elsif c == '\\'
		escapes += 1
	else
		escapes = 0
	end

	if (c != ' ' && c != "\t" && c != "\n") || string
		output += c
	end
end

puts output
