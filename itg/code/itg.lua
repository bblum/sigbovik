function read_input(filename)
	local lines = {}
	local i = 0
	local f = io.open(filename, "r")
	while true do
		local line = f:read("*line")
		if not line then break end
		i = i + 1
		lines[i] = line
	end
	return lines, i
end

lines, n = read_input("test.sm")
print(n)
for i,line in ipairs(lines) do
	print(i .. " " .. line)
end
