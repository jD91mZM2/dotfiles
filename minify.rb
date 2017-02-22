input = $stdin.read;

output = "";
string = false;
escapes = 0;
for c in input.split("")
	if c == '"' && escapes % 2 == 0 then
		string = !string;
		escapes = 0;
	elsif c == '\\' then
		escapes += 1;
	else
		escapes = 0;
	end

	if (c != ' ' && c != "\t" && c != "\n") || string then
		output += c;
	end
end

puts output;
