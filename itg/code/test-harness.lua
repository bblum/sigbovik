assert(loadfile("SL-ChartParser.lua"))()

-- override io routine
GetSimfileString = function(Steps)
	local filename = arg[1]
	if not filename then
		assert(false, "supply simfile arg")
	end
	local filetype = filename:match("[^.]+$"):lower()
	if not (filetype=="ssc" or filetype=="sm") then
		assert(false, "file not ssc or sm: " .. filename)
	end

	contents = ""
	for line in io.lines(filename) do
		contents = contents .. line .. "\n"
	end
	return contents, filetype
end

mock = {}
mock.superSecretDebugMode = {}
function mock:GetStepsType()   return "dance-single" end
function mock:GetDifficulty()  return "Challenge" end
function mock:GetDescription() return "hax and sploits" end
function ToEnumShortString(x)  return x end

-- production order
-- xo, fs, ss, jk, br = GetSongStatsSIGBOVIKEdition(mock)
-- print(xo .. "," .. fs .. "," .. ss .. "," .. jk .. "," .. br)
-- test order
sj, ns, xo, fs, jk, ds, ss, br = GetSongStatsSIGBOVIKEdition(mock)
print(sj .. "," .. ns .. "," .. xo .. "," .. fs .. "," .. jk .. "," .. ds .. "," .. ss .. "," .. br)
