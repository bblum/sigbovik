GetSimfileString = function(steps)

	-- steps:GetFilename() returns the filename of the sm or ssc file, including path, as it is stored in SM's cache
	local filename = steps:GetFilename()
	if not filename then return end

	-- get the file extension like "sm" or "SM" or "ssc" or "SSC" or "sSc" or etc.
	-- convert to lowercase
	local filetype = filename:match("[^.]+$"):lower()
	-- if file doesn't match "ssc" or "sm", it was (hopefully) something else (.dwi, .bms, etc.)
	-- that isn't supported by SL-ChartParser
	if not (filetype=="ssc" or filetype=="sm") then return end

	-- create a generic RageFile that we'll use to read the contents
	-- of the desired .ssc or .sm file
	local f = RageFileUtil.CreateRageFile()
	local contents

	-- the second argument here (the 1) signals
	-- that we are opening the file in read-only mode
	if f:Open(filename, 1) then
		contents = f:Read()
	end

	-- destroy the generic RageFile now that we have the contents
	f:destroy()
	return contents, filetype
end

-- ----------------------------------------------------------------
-- SOURCE: https://github.com/JonathanKnepp/SM5StreamParser

-- Which note types are counted as part of the stream?
local TapNotes = {1,2,4}


-- Utility function to replace regex special characters with escaped characters
local regexEncode = function(var)
	return (var:gsub('%%', '%%%'):gsub('%^', '%%^'):gsub('%$', '%%$'):gsub('%(', '%%('):gsub('%)', '%%)'):gsub('%.', '%%.'):gsub('%[', '%%['):gsub('%]', '%%]'):gsub('%*', '%%*'):gsub('%+', '%%+'):gsub('%-', '%%-'):gsub('%?', '%%?'))
end

-- GetSimfileChartString() accepts four arguments:
--    SimfileString - the contents of the ssc or sm file as a string
--    StepsType     - a string like "dance-single" or "pump-double"
--    Difficulty    - a string like "Beginner" or "Challenge" or "Edit"
--    Filetype      - either "sm" or "ssc"
--
-- GetSimfileChartString() returns one value:
--    NoteDataString, a substring from SimfileString that contains the just the requested note data

local GetSimfileChartString = function(SimfileString, StepsType, Difficulty, StepsDescription, Filetype)
	local NoteDataString = nil

	-- ----------------------------------------------------------------
	-- StepMania uses each steps' "Description" attribute to uniquely
	-- identify Edit charts. (This is important, because there can be more
	-- than one Edit chart.)
	--
	-- ssc files use a dedicated #DESCRIPTION for this purpose
	-- but sm files have the description as part of an inline comment like
	--
	-- //---------------dance-single - test----------------
	--
	-- that^ edit stepchart would have a description of "test"
	-- ----------------------------------------------------------------

	if Filetype == "ssc" then
		-- SSC File
		-- Loop through each chart in the SSC file
		for chart in SimfileString:gmatch("#NOTEDATA.-#NOTES:[^;]*") do
			-- Find the chart that matches our difficulty and game type
			if(chart:match("#STEPSTYPE:"..regexEncode(StepsType)) and chart:match("#DIFFICULTY:"..regexEncode(Difficulty))) then
				-- ensure that we've located the correct edit stepchart within the ssc file
				-- there can be multiple Edit stepcharts but each is guaranteed to have a unique #DESCIPTION tag
				if (Difficulty ~= "Edit") or (Difficulty=="Edit" and chart:match("#DESCRIPTION:"..regexEncode(StepsDescription))) then
					-- Find just the notes
					NoteDataString = chart:match("#NOTES:[\r\n]+([^;]*)\n?$")
					-- remove possible comments
					NoteDataString = NoteDataString:gsub("\\[^\r\n]*", "")
					NoteDataString = NoteDataString:gsub("//[^\r\n]*", "")
					-- put the semicolon back so that the line-by-line loop knows when to stop
					NoteDataString = NoteDataString .. ";"
					break
				end
			end
		end
	elseif Filetype == "sm" then
		-- SM FILE
		-- Loop through each chart in the SM file
		for chart in SimfileString:gmatch("#NOTES[^;]*") do
			-- split the entire chart string into pieces on ":"
			local pieces = {}
			for str in chart:gmatch("[^:]+") do
				pieces[#pieces+1] = str
			end

			-- the pieces table should contain 7 numerically indexed items
			-- 2, 4, and 7 are the indices we care about for finding the correct chart
			-- index 2 will contain the steps_type (like "dance-single")
			-- index 4 will contain the difficulty (like "challenge")

			-- use gsub to scrub out line breaks (and other irrelevant characters?)
			local st = pieces[2]:gsub("[^%w-]", "")
			local diff = pieces[4]:gsub("[^%w]", "")
			-- trim leading whitespace
			local description = pieces[3]:gsub("^%s*", "")

			-- if this particular chart's steps_type matches the desired StepsType
			-- and its difficulty string matches the desired Difficulty
			if (st == StepsType) and (diff == Difficulty) and (diff ~= "Edit" or description == StepsDescription) then
				-- then index 7 contains the notedata that we're looking for
				-- use gsub to remove comments, store the resulting string,
				-- and break out of the chart loop now
				NoteDataString = pieces[7]:gsub("//[^\r\n]*","") .. ";"
				break
			end
		end
	end

	return NoteDataString
end

-- Figure out which measures are considered a stream of notes
local getStreamMeasures = function(measuresString, notesPerMeasure)
	-- Make our stream notes array into a string for regex
	local TapNotesString = ""
	for i, v in ipairs(TapNotes) do
		TapNotesString = TapNotesString .. v
	end
	TapNotesString = "["..TapNotesString.."]"

	-- Which measures are considered a stream?
	local streamMeasures = {}

	-- Keep track of the measure and its timing (8ths, 16ths, etc)
	local measureCount = 1
	local measureTiming = 0
	-- Keep track of the notes in a measure
	local measureNotes = {}

	-- Loop through each line in our string of measures, trimming potential leading whitespace (thanks, TLOES/Mirage Garden)
	for line in measuresString:gmatch("[^%s*\r\n]+")
	do
		-- If we hit a comma or a semi-colon, then we've hit the end of our measure
		if(line:match("^[,;]%s*")) then
			-- Does this measure contain a stream of notes based on our notesPerMeasure global?
			if(#measureNotes >= notesPerMeasure) then
				table.insert(streamMeasures, measureCount)
			end

			-- Reset iterative variables
			measureTiming = 0
			measureCount = measureCount + 1
			measureNotes = {}
		else
			-- increment the measure timing
			measureTiming = measureTiming + 1

			-- Is this a note?
			if(line:match(TapNotesString)) then
				table.insert(measureNotes, measureTiming)
			end
		end
	end

	return streamMeasures, measureCount
end

-- Get the start/end of each stream and break sequence in our table of measures
local getStreamSequences = function(streamMeasures, totalMeasures)
	local streamSequences = {}
	-- Count every measure as stream/non-stream.
	-- We can then later choose how we want to display the information.
	local measureSequenceThreshold = 1

	local counter = 1
	local streamEnd = nil

	-- First add an initial break if it's larger than measureSequenceThreshold
	if #streamMeasures > 0 then
		local breakStart = 0
		local k, v = next(streamMeasures) -- first element of a table
		local breakEnd = streamMeasures[k] - 1
		if (breakEnd - breakStart >= measureSequenceThreshold) then
			table.insert(streamSequences,
				{streamStart=breakStart, streamEnd=breakEnd, isBreak=true})
		end
	end

	-- Which sequences of measures are considered a stream?
	for k,v in pairs(streamMeasures) do
		local curVal = streamMeasures[k]
		local nextVal = streamMeasures[k+1] and streamMeasures[k+1] or -1

		-- Are we still in sequence?
		if curVal + 1 == nextVal then
			counter = counter + 1
			streamEnd = curVal + 1
		else
			-- Found the first section that counts as a stream
			if(counter >= measureSequenceThreshold) then
				if streamEnd == nil then
					streamEnd = curVal
				end
				local streamStart = (streamEnd - counter)
				-- Add the current stream.
				table.insert(streamSequences,
					{streamStart=streamStart, streamEnd=streamEnd, isBreak=false})
			end

			-- Add any trailing breaks if they're larger than measureSequenceThreshold
			local breakStart = curVal
			local breakEnd = (nextVal ~= -1) and nextVal - 1 or totalMeasures
			if (breakEnd - breakStart >= measureSequenceThreshold) then
				table.insert(streamSequences,
					{streamStart=breakStart, streamEnd=breakEnd, isBreak=true})
			end
			counter = 1
			streamEnd = nil
		end
	end

	return streamSequences
end


-- GetNPSperMeasure() accepts two arguments:
-- 		Song, a song object provided by something like GAMESTATE:GetCurrentSong()
-- 		Steps, a steps object provided by something like GAMESTATE:GetCurrentSteps(player)
--
-- GetNPSperMeasure() returns two values:
--		PeakNPS, a number representing the peak notes-per-second for the given stepchart
--			This is an imperfect measurement, as we sample the note density per-second-per-measure, not per-second.
--			It is (unlikely but) possible for the true PeakNPS to be spread across the boundary of two measures.
--		Density, a numerically indexed table containing the notes-per-second value for each measure
--			The Density table is indexed from 1 (as Lua tables go); simfile charts, however, start at measure 0.
--			So if you're looping through the Density table, subtract 1 from the current index to get the
--			actual measure number.

GetNPSperMeasure = function(Song, Steps)
	if Song==nil or Steps==nil then return end

	local SongDir = Song:GetSongDir()
	local SimfileString, Filetype = GetSimfileString( Steps )
	if not SimfileString then return end

	-- StepsType, a string like "dance-single" or "pump-double"
	local StepsType = ToEnumShortString( Steps:GetStepsType() ):gsub("_", "-"):lower()
	-- Difficulty, a string like "Beginner" or "Challenge"
	local Difficulty = ToEnumShortString( Steps:GetDifficulty() )
	-- an arbitary but unique string provded by the stepartist, needed here to identify Edit charts
	local StepsDescription = Steps:GetDescription()

	-- Discard header info; parse out only the notes
	local ChartString = GetSimfileChartString(SimfileString, StepsType, Difficulty, StepsDescription, Filetype)
	if not ChartString then return end

	-- Make our stream notes array into a string for regex
	local TapNotesString = ""
	for i, v in ipairs(TapNotes) do
		TapNotesString = TapNotesString .. v
	end
	TapNotesString = "["..TapNotesString.."]"

	-- the main density table, indexed by measure number
	local Density = {}
	-- Keep track of the measure
	local measureCount = 0
	-- Keep track of the number of notes in the current measure while we iterate
	local NotesInThisMeasure = 0

	local NPSforThisMeasure, PeakNPS, BPM = 0, 0, 0
	local TimingData = Steps:GetTimingData()

	-- Loop through each line in our string of measures, trimming potential leading whitespace (thanks, TLOES/Mirage Garden)
	for line in ChartString:gmatch("[^%s*\r\n]+") do

		-- If we hit a comma or a semi-colon, then we've hit the end of our measure
		if (line:match("^[,;]%s*")) then

			DurationOfMeasureInSeconds = TimingData:GetElapsedTimeFromBeat((measureCount+1)*4) - TimingData:GetElapsedTimeFromBeat(measureCount*4)

			-- FIXME: We subtract the time at the current measure from the time at the next measure to determine
			-- the duration of this measure in seconds, and use that to calculate notes per second.
			--
			-- Measures *normally* occur over some positive quantity of seconds.  Measures that use warps,
			-- negative BPMs, and negative stops are normally reported by the SM5 engine as having a duration
			-- of 0 seconds, and when that happens, we safely assume that there were 0 notes in that measure.
			--
			-- This doesn't always hold true.  Measures 48 and 49 of "Mudkyp Korea/Can't Nobody" use a properly
			-- timed negative stop, but the engine reports them as having very small but positive durations
			-- which erroneously inflates the notes per second calculation.

			if (DurationOfMeasureInSeconds == 0) then
				NPSforThisMeasure = 0
			else
				NPSforThisMeasure = NotesInThisMeasure/DurationOfMeasureInSeconds
			end

			-- measureCount in SM truly starts at 0, but Lua's native ipairs() iterator needs indexed tables
			-- that start at 1.   Add 1 now so the table behaves and subtract 1 later when drawing the histogram.
			Density[measureCount+1] = NPSforThisMeasure

			-- determine whether this measure contained the PeakNPS
			if NPSforThisMeasure > PeakNPS then PeakNPS = NPSforThisMeasure end
			-- increment the measureCount
			measureCount = measureCount + 1
			-- and reset NotesInThisMeasure
			NotesInThisMeasure = 0
		else
			-- does this line contain a note?
			if (line:match(TapNotesString)) then
				NotesInThisMeasure = NotesInThisMeasure + 1
			end
		end
	end

	return PeakNPS, Density
end



GetStreams = function(Steps, StepsType, Difficulty, NotesPerMeasure)

	local SimfileString, Filetype = GetSimfileString( Steps )
	if not SimfileString then return end

	-- an arbitary but unique string provded by the stepartist, needed here to identify Edit charts
	local StepsDescription = Steps:GetDescription()

	-- Parse out just the contents of the notes
	local ChartString = GetSimfileChartString(SimfileString, StepsType, Difficulty, StepsDescription, Filetype)
	if not ChartString then return end

	-- Which measures have enough notes to be considered as part of a stream?
	local StreamMeasures, totalMeasures = getStreamMeasures(ChartString, NotesPerMeasure)

	-- Which sequences of measures are considered a stream?
	return (getStreamSequences(StreamMeasures, totalMeasures))
end

-- See the "Which Stepcharts are Crossoveriest" series from SIGBOVIK, esp. 2017 and 2019.
GetSongStatsSIGBOVIKEdition = function(Steps)
	-- the algorithm supports only 4-panel
	-- TODO: make caller check for this return value and fall back to hands/mines/etc
	local StepsType = ToEnumShortString(Steps:GetStepsType()):gsub("_", "-"):lower()
	if (StepsType ~= "dance-single") then return 69, 420, 0, 0, 0 end

	-- parse the chart
	-- this setup stuff is largely copied from GetNPSperMeasure
	local SimfileString, FileType = GetSimfileString(Steps)
	if not SimfileString then return 69, 421, 0, 0, 0 end

	local Difficulty = ToEnumShortString(Steps:GetDifficulty())
	local StepsDescription = Steps:GetDescription()
	local ChartString = GetSimfileChartString(SimfileString, StepsType, Difficulty, StepsDescription, FileType)
	if not ChartString then return 69, 422, 0, 0, 0 end

	function debug(msg)
		if Steps.superSecretDebugMode then
			print(msg)
		end
	end

	debug(ChartString)

	local RegexStep = "[124]"
	local RegexAny = "." -- "[%dM]" -- performance, i think
	-- NB: not sure if charts can have junk eg "0000 // whatever", so not matching `$`
	local RegexL = "^" .. RegexStep .. RegexAny .. RegexAny .. RegexAny
	local RegexD = "^" .. RegexAny .. RegexStep .. RegexAny .. RegexAny
	local RegexU = "^" .. RegexAny .. RegexAny .. RegexStep .. RegexAny
	local RegexR = "^" .. RegexAny .. RegexAny .. RegexAny .. RegexStep

	-- output counters
	local NumCrossovers = 0
	local NumFootswitches = 0
	local NumSideswitches = 0
	local NumJacks = 0
	local NumBrackets = 0
	local NumHeuristicDoublesteps = 0
	local NumSteps = 0
	local NumStepsAndJumps = 0

	-- transient algorithm state
	local LastFoot = false -- false = left, true = right
	local WasLastStreamFlipped = false
	local LastStep -- Option<LDUR>
	local LastRepeatedFoot -- Option<LDUR>
	-- TODO: microoptimize(?) by counting `NumLRCrossed` explicitly here,
	-- and maybe even eg `NumConsecutiveFlipped`. test on longass trancemania songs
	local StepsLR = {}

	-- bracket jump stuff
	-- tracks the last arrow(s) (can be plural in the case of brackets themselves)
	-- that each of the L and R feet was last on, respectively; however,
	-- note that this can be flipped while recording a (unflipped?) stream...
	local lastArrowL = "X"
	local lastArrowR = "X"
	-- ...so these track the true state of what happened (which become known only
	-- after deciding whether to flip, hence are updated every CommitStream)
	-- NB: these are "X" not "" b/c we use them in `match()`, and "" shouldn't match
	local trueLastArrowL = "X"
	local trueLastArrowR = "X"
	local trueLastFoot = nil

	-- TODO(bracket) - figure out what corner cases this is needed for...?
	-- local justBracketed = false -- used for tiebreaks

	-- TODO(bracket) - make take `tiebreakFoot` arg
	function CommitStream()
		local ns = #StepsLR
		local nx = 0
		for _i, step in ipairs(StepsLR) do
			-- count crossed-over steps given initial footing
			if not step then nx = nx + 1 end
		end

		local needFlip = false
		if nx * 2 > ns then
			-- easy case - more than half the L/R steps in this stream were crossed over,
			-- so we guessed the initial footing wrong and need to flip the stream.
			needFlip = true
		elseif nx * 2 == ns then
			-- exactly half crossed over. note that flipping the stream will introduce
			-- a jack (i.e. break the alternating-feet assumption) on the first note,
			-- whereas leaving it as is will footswitch that note. break the tie by
			-- looking at history to see if the chart is already more jacky or switchy.
			-- (otoh, the reverse applies if the previous stream was also flipped.)
			if NumFootswitches > NumJacks then
				needFlip = LastFlip -- match flipness of last chunk -> footswitch
			else
				needFlip = not LastFlip -- don't match -> jack
			end
		end

		-- now that we know the correct flip, see if the stream needs split.
		-- if "too much" of the stream is *completely* crossed over, force a
		-- double-step there by splitting the stream to stay facing forward.
		-- heuristic value (9) chosen by inspection on Subluminal - After Hours.
		local splitIndex -- Option<int>
		local splitFirstUncrossedStepIndex -- Option<Int>
		local numConsecutiveCrossed = 0
		for i, step in ipairs(StepsLR) do
			local stepIsCrossed = step == needFlip
			if not splitIndex then -- lua doesn't have `break` huh. ok
				if stepIsCrossed then
					numConsecutiveCrossed = numConsecutiveCrossed + 1
					if numConsecutiveCrossed == 9 then
						splitIndex = i - 8 -- beware the 1-index
					end
				else
					numConsecutiveCrossed = 0
				end
			elseif not splitFirstUncrossedStepIndex then
				-- also search for the first un-crossed step after the fux section,
				-- which will be used below in the `splitIndex == 1` case.
				if not stepIsCrossed then
					splitFirstUncrossedStepIndex = i
				end
			end
		end

		if splitIndex then
			NumHeuristicDoublesteps = NumHeuristicDoublesteps + 1
			-- note that since we take O(n) to compute `needFlip`, and then we might
			-- do repeated work scanning already-analyzed ranges of `StepsLR` during
			-- the recursive call here, it's technically possible for a worst case
			-- performance of O(n^2 / 18), if the whole chart fits this pattern.
			-- but this is expected to be pretty rare to happen even once so probs ok.
			-- TODO: optmz that ^^^ by using a separate explicit counter for `nx`.
			if splitIndex == 1 then
				-- prevent infinite splittage if the fux section starts immediately.
				-- in that case split instead at the first non-crossed step.
				-- the next index is guaranteed to be set in this case, b/c i said so.
				splitIndex = splitFirstUncrossedStepIndex -- .unwrap()
			end
			-- split that sucker
			local StepsLR1 = {}
			local StepsLR2 = {}
			for i, step in ipairs(StepsLR) do
				if i < splitIndex then
					StepsLR1[#StepsLR1+1] = step
				else
					StepsLR2[#StepsLR2+1] = step
				end
			end
			-- recurse for each split half
			StepsLR = StepsLR1
			CommitStream()
			LastRepeatedFoot = nil
			StepsLR = StepsLR2
			CommitStream()
		else
			-- no heuristic doublestep splittage necessary; update the stats.
			if needFlip then
				NumCrossovers = NumCrossovers + ns - nx
			else
				NumCrossovers = NumCrossovers + nx
			end

			if LastRepeatedFoot then
				if needFlip == LastFlip then
					NumFootswitches = NumFootswitches + 1
					if LastRepeatedFoot == "L" or LastRepeatedFoot == "R" then
						NumSideswitches = NumSideswitches + 1
					end
				else
					NumJacks = NumJacks + 1
				end
			end

			StepsLR = {}
			LastFlip = needFlip

			-- merge the (flip-ambiguous) last-arrow tracking into the source of truth
			-- TODO(bracket) - do you need to check if the `lastArrow`s are empty, like
			-- the hs version does? i hypothesize it actually makes no difference.
			if ns > 0 then -- XXX
				if needFlip then
					-- LastFoot is a tristate so can't just copy the bool
					if LastFoot then trueLastFoot = "L" else trueLastFoot = "R" end
					trueLastArrowL = lastArrowR
					trueLastArrowR = lastArrowL
				else
					if LastFoot then trueLastFoot = "R" else trueLastFoot = "L" end
					trueLastArrowL = lastArrowL
					trueLastArrowR = lastArrowR
				end
			end
			debug(
				"CommitStream..."
				.. " ns: " .. ns
				.. " nx: " .. nx
				.. " needFlip: " .. tostring(needFlip)
			)
			debug("            ... trueLastFoot: " .. (trueLastFoot or "nil"))
			debug("            ... lastArrowL: " .. lastArrowL)
			debug("            ... lastArrowR: " .. lastArrowR)
			debug("            ... trueLastArrowL: " .. trueLastArrowL)
			debug("            ... trueLastArrowR: " .. trueLastArrowR)
			lastArrowL = ""
			lastArrowR = ""
		end
	end

	-- TODO - why \r\n, how about testing w a not-dos-formatted chart? `\r?` ?
	for line in ChartString:gmatch("[^%s*\r\n]+") do
		if line:match(RegexStep) then
			-- TODO this can be a list and you can index it maybe
			local step = ""
			if line:match(RegexL) then step = step .. "L" end
			if line:match(RegexD) then step = step .. "D" end
			if line:match(RegexU) then step = step .. "U" end
			if line:match(RegexR) then step = step .. "R" end

			if step:len() == 1 then
				-- normal step
				NumSteps = NumSteps + 1
				NumStepsAndJumps = NumStepsAndJumps + 1
				if LastStep and step == LastStep then
					-- jack or footswitch
					CommitStream()
					LastRepeatedFoot = step
				end

				-- a normal streamy step
				LastStep = step
				-- switch feet
				LastFoot = not LastFoot
				-- record whether we stepped on a matching or crossed-over L/R arrow
				-- TODO: check yes/not true/false left/right parity here (vs .hs/.cpps)
				local crossed
				if step == "L" then
					crossed = LastFoot
					StepsLR[#StepsLR+1] = not LastFoot
				elseif step == "R" then
					crossed = not LastFoot
					StepsLR[#StepsLR+1] = LastFoot
				end
				-- regardless, record what arrow the foot stepped on (for brackets l8r)
				if LastFoot then
					lastArrowR = step
				else
					lastArrowL = step
				end
				debug(
					"normal step (" .. step .. ")..."
					.. " lastFoot: " .. tostring(LastFoot)
					.. " crossed: " .. tostring(crossed)
					.. " lastArrowL: " .. lastArrowL
					.. " lastArrowR: " .. lastArrowR
				)
			elseif step:len() > 1 then
				-- jump
				NumStepsAndJumps = NumStepsAndJumps + 1
				-- TODO(bracket) - make stream able to continue thru a bracket jump
				CommitStream()
				LastStep = nil
				LastRepeatedFoot = nil

				if step:len() == 2 then
					local isBracketLeft  = step:match("L[^R]")
					local isBracketRight = step:match("[^L]R")
					if isBracketLeft or isBracketRight then
						-- possibly bracketable
						if isBracketLeft and (not trueLastFoot or trueLastFoot == "R") then
							-- check for interference from the right foot
							if not step:match(trueLastArrowR) then
								NumBrackets = NumBrackets + 1
								-- this prevents e.g. "LD bracket, DR also bracket"
								trueLastArrowL = step
							else
								-- right foot is in the way; hafta step w both feet
								-- NB: i don't think resetting the lastarrows is
								-- actually necessary here since i think any chart
								-- where it would matter would already be ambiguous
							end
						elseif isBracketRight and (not trueLastFoot or trueLastFoot == "L") then
							-- check for interference from the left foot
							if not step:match(trueLastArrowL) then
								NumBrackets = NumBrackets + 1
								-- this prevents e.g. "DR bracket, LD also bracket"
								trueLastArrowR = step
							else
								-- (same as above)
							end
						end
					else
						-- LR or DU
						trueLastArrowL = "X"
						trueLastArrowR = "X"
						-- TODO(bracket) - decideDUFacing?
					end
				else
					-- triple/quad - always gotta bracket these
					NumBrackets = NumBrackets + 1
				end
			end
		end
	end
	CommitStream()

	if Steps.superSecretDebugMode then
		return NumStepsAndJumps, NumSteps, NumCrossovers, NumFootswitches, NumJacks, NumHeuristicDoublesteps, NumSideswitches, NumBrackets
	else
		return NumCrossovers, NumFootswitches, NumSideswitches, NumJacks, NumBrackets
	end
end
