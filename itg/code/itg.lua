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
			title = line:gsub(".*#TITLE:(.*);\r?$", "%1")
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

-- "Prelude"
function append(list1, list2) -- note: in-place append. O(length(list2)) (i hope)
	for _,val in pairs(list2) do
		table.insert(list1, val)
	end
end

function takeWhile(list, f)
	result = {}
	for _,val in pairs(list) do
		if (f(val)) then
			table.insert(result, val)
		else
			break
		end
	end
	return result
end

function dropWhile(list, f)
	result = {}
	ready = false
	for _,val in pairs(list) do
		if not ready and not f(val) then
			ready = true
		end
		if ready then
			table.insert(result, val)
		end
	end
	return result
end

function dropWhileIncluding(list, f) -- as dropWhile but loses the 1st matching element
	result = {}
	ready = false
	for _,val in pairs(list) do
		if not ready then
			if not f(val) then
				ready = true
			end
		elseif ready then
			table.insert(result, val)
		end
	end
	return result
end

function map(list, f)
	result = {}
	for _,val in pairs(list) do
		table.insert(result, f(val))
	end
	return result
end

function filter(list, f)
	result = {}
	for _,val in pairs(list) do
		if f(val) then
			table.insert(result, val)
		end
	end
	return result
end

function findIndex(list, f)
	for i,val in pairs(list) do
		if (f(val)) then
			return i
		end
	end
	return nil
end

-- charts :: String -> [String] -> [(String, [String])]
function charts(title, contents)
	
end

-- note: inserting 'nil' into a table is a noop; it doesn't even allocate the index
test = filter({10, 20, 30, 40, 30, 20, 10}, function(x) return x>25 end)
for i,val in pairs(test) do
	print("test: " .. i .. ", " .. tostring(val))
end
test2 = findIndex({10, 20, 30, 40, 30, 20, 10}, function(x) return x>55 end)
if test2 == nil then
	print("crap")
else
	print(test2)
end
