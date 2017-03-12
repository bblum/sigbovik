import Data.List
import Data.Char
import Data.Maybe

data Step = L | D | U | R | Jump deriving Eq

data AnalysisState = S { steps :: Int, xovers :: Int, switches :: Int, jacks :: Int,
                         lastStep :: Maybe Step, doubleStep :: Bool, lastFlip :: Bool,
                         lastFoot :: Bool, stepsLR :: [Bool] }

commitStream :: AnalysisState -> AnalysisState
commitStream s = s { xovers   = xovers   s + if f then ns - nx else nx,
                     switches = switches s + fromEnum (f == lastFlip s && doubleStep s),
                     jacks    = jacks    s + fromEnum (f /= lastFlip s && doubleStep s),
                     lastFlip = f, stepsLR = [] }
    where ns = length $ stepsLR s
          nx = length $ filter not $ stepsLR s
          -- if more than half the L/R steps in this stream were crossed over,
          -- then we got the footing backwards and need to flip the stream.
          -- as a tiebreaker, flip if the stream is already more jacky than
          -- footswitchy, i.e., if past streams flipped more often than not.
          f = nx * 2 > ns || nx * 2 == ns && ((jacks s > switches s) /= lastFlip s)

analyzeStep :: AnalysisState -> Step -> AnalysisState
analyzeStep s step
    -- a jump resets the footing, so the next step can be stepped with either
    -- foot. commit the stream so far to treat it separately from what follows.
    -- bracket-jumps are, of course, future work.
    | step == Jump = (commitStream s) { lastStep = Nothing, doubleStep = False }
    -- two steps on the same arrow might be a jack, or might be a footswitch.
    -- to figure out which, commit the stream so far, and begin a new stream
    -- whose footing will retroactively determine how to foot this step.
    -- also, unlike jumps, this step gets counted as part of the next stream.
    | lastStep s == Just step = stream (commitStream s) { doubleStep = True }
    -- a normal streamy step.
    | otherwise = stream s
    where foot = not $ lastFoot s
          -- record whether we stepped on a matching or crossed-over L/R arrow.
          addStep ft L steps = ft:steps
          addStep ft R steps = (not ft):steps
          addStep ft _ steps = steps -- U/D don't help to determine L/R footing.
          stream s = s { steps = steps s + 1, lastStep = Just step, lastFoot = foot,
                         stepsLR = addStep foot step $ stepsLR s }

analyze :: [Step] -> AnalysisState
analyze = commitStream . foldl analyzeStep (S 0 0 0 0 Nothing False False False [])

-- turns a line of stepchart, eg "0100", into a Step, e.g. "D"
-- in stepchart-ese: 1 = tap; 2 = hold; 3 = hold release; 4 = roll; M = mine
stepify :: String -> Maybe Step
stepify = step . map fst . filter snd . zip [L,D,U,R] . map (flip elem "124")
    where step [] = Nothing
          step [s] = Just s
          step ss = Just Jump

-- handles chart metadata and formats the analysis result into tsv
process :: (String, [String]) -> String
process (title,(name:diff:feet:_:rest)) = intercalate "\t" $ metadata ++ result
    where -- chop off the trailing ":"
          trim = reverse . tail . dropWhile isSpace . reverse . dropWhile isSpace
          metadata = map trim [title,name,diff,feet]
          finalState = analyze $ catMaybes $ map stepify rest
          result = map (show . ($ finalState)) [steps, xovers, switches, jacks]

-- splits up the input file by the "#NOTES:" lines
charts :: String -> [String] -> [(String, [String])]
charts t [] = []
charts t xs = chart ++ charts title rest
    where title = fromMaybe t $ drop 7 <$> find (isPrefixOf "#TITLE:") xs
          -- get everything after the first "#NOTES"
          notnotes = not . isInfixOf "#NOTES:"
          stuff = tail $ dropWhile notnotes xs
          -- split this chart from the rest
          c0   = takeWhile notnotes stuff
          rest = dropWhile notnotes stuff
          -- accept only singles charts
          single = isInfixOf "dance-single:"
          chart = if any single c0 then [(title, tail $ dropWhile (not . single) c0)] else []

main = interact $ unlines . map process . charts "" . lines
