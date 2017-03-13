import Data.List
import Data.Char
import Data.Maybe

data Step = L | D | U | R | Jump deriving Eq

data AnalysisState = S { steps :: Int, xovers :: Int, switches :: Int,
                         jacks :: Int, doubles :: Int, xoverfs :: Int,
                         lastStep :: Maybe Step, lastJack :: Maybe Step,
                         lastFlip :: Bool, lastFoot :: Bool, stepsLR :: [Bool] }

commitStream :: AnalysisState -> AnalysisState
commitStream s = maybe s0 splitStream splitIndex
    where ns = length $ stepsLR s
          nx = length $ filter not $ stepsLR s
          -- if more than half the L/R steps in this stream were crossed over,
          -- then we got the footing backwards and need to flip the stream.
          -- as a tiebreaker, flip if the chart is already more jacky than
          -- footswitchy, i.e., if past streams flipped more often than not.
          f = nx * 2 > ns || nx * 2 == ns && ((switches s > jacks s) == lastFlip s)
          -- if "too much" of the stream is *completely* crossed-over, force
          -- a double-step there by splitting the stream to stay facing forward.
          -- heuristic value was chosen by inspection on Subluminal - After Hours.
          splitIndex = findIndex (isPrefixOf $ replicate 9 f) $ tails $ stepsLR s
          -- prevent infinite splittage if the fux section starts immediately;
          -- in that case split instead at fux's end (i.e., first non-crossed step).
          splitStream 0 = splitStream $ fromJust $ findIndex (/= f) $ stepsLR s
          splitStream i = s2 { doubles = doubles s2 + 1 }
              where (steps1, steps2) = splitAt i $ stepsLR s
                    s1 = commitStream s  { stepsLR = steps1 }
                    s2 = commitStream s1 { stepsLR = steps2, lastJack = Nothing }
          -- normal case; consider the whole stream at once
          jack = lastJack s /= Nothing
          jackLR = lastJack s == Just L || lastJack s == Just R
          s0 = s { xovers   = xovers   s + if f then ns - nx else nx,
                   switches = switches s + fromEnum (f == lastFlip s && jack),
                   jacks    = jacks    s + fromEnum (f /= lastFlip s && jack),
                   xoverfs  = xoverfs  s + fromEnum (f == lastFlip s && jackLR),
                   lastFlip = f, stepsLR = [] }

analyzeStep :: AnalysisState -> Step -> AnalysisState
analyzeStep s step
    -- a jump resets the footing, so the next step can be stepped with either
    -- foot. commit the stream so far to treat it separately from what follows.
    -- bracket-jumps are, of course, future work.
    | step == Jump = (commitStream s) { lastStep = Nothing, lastJack = Nothing }
    -- two steps on the same arrow might be a jack, or might be a footswitch.
    -- to figure out which, commit the stream so far, and begin a new stream
    -- whose footing will retroactively determine how to foot this step.
    -- also, unlike jumps, this step gets counted as part of the next stream.
    | lastStep s == Just step = stream (commitStream s) { lastJack = Just step }
    -- a normal streamy step.
    | otherwise = stream s
    where foot = not $ lastFoot s
          -- record whether we stepped on a matching or crossed-over L/R arrow.
          addStep ft L steps = steps ++ [ft]
          addStep ft R steps = steps ++ [not ft]
          addStep ft _ steps = steps -- U/D don't help to determine L/R footing.
          stream s = s { steps = steps s + 1, lastStep = Just step, lastFoot = foot,
                         stepsLR = addStep foot step $ stepsLR s }

analyze :: [Step] -> AnalysisState
analyze = commitStream . foldl analyzeStep (S 0 0 0 0 0 0 Nothing Nothing False False [])

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
          finalState = analyze $ catMaybes $ map (stepify . dropWhile isSpace) rest
          result = map (show . ($ finalState)) [steps, xovers, switches,
                                                jacks, doubles, xoverfs]

-- splits up the input file by the "#NOTES:" lines
charts :: String -> [String] -> [(String, [String])]
charts t [] = []
charts t xs = chart ++ charts title rest
    where title = fromMaybe t $ drop 7 <$> dropWhile isSpace <$> find (isInfixOf "#TITLE:") xs
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
