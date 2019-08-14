function read_input(filename)
	local lines = {}
	local f = io.open(filename, "r")
	while true do
		local line = f:read("*line")
		if not line then break end
		table.insert(lines, line)
	end
	return lines
end

function parse_chart(chart_contents)
	print("parse chart", #chart_contents)
	return #chart_contents
end

-- returns (title, list of charts)
-- parse_sm :: [String] -> (String, [[String]]
function parse_sm(sm_contents)
	local i = 1
	while i <= #sm_contents do
		local line = sm_contents[i]
		if line:find("#TITLE:") then
			title = line:gsub(".*#TITLE:(.*);$", "%1")
			break
		end
		i = i + 1
	end
	-- ff to the first "#NOTES:"
	while i <= #sm_contents do
		local line = sm_contents[i]
		i = i + 1
		if line:find("#NOTES:") then break end
	end
	-- find charts
	local charts = {}
	local chart = {}
	local is_dance_single = true
	local parse_and_add_chart = function()
		if is_dance_single then
			table.insert(charts, parse_chart(chart))
			chart = {}
		else
			assert(#chart == 0)
		end
	end
	while i <= #sm_contents do
		local line = sm_contents[i]
		if line:find("#NOTES:") then
			-- process chart contents and reset it for the next difficulty
			parse_and_add_chart()
			-- process the next line too: it should be the type of chart
			-- if it's not dance single then skip it all
			i = i + 1
			line = sm_contents[i]
			is_dance_single = not not line:find("dance-single:")
		elseif is_dance_single then
			table.insert(chart, line)
		end
		i = i + 1
	end
	parse_and_add_chart()
	return title, charts
end


lines = read_input(arg[1])
title, charts = parse_sm(lines)
print(title .. " has " .. #charts .. " charts!")
