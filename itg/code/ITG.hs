import Data.List
import Data.Char
import Data.Maybe

data Arrow = L| D | U | R deriving (Ord, Eq)

data Jump = LD | LU | DR | UR | Other deriving (Ord, Eq)

data Step = A Arrow | J Jump deriving (Ord, Eq)

data Foot = LeftFoot | RightFoot deriving (Ord, Eq)

alternate LeftFoot = RightFoot
alternate RightFoot = LeftFoot

-- TODO add a field to track what the last arrow the left foot and right foot were on
-- if you commitstream flipped, you need to flip those around too
data AnalysisState = S { steps :: Int, xovers :: Int, switches :: Int,
                         jacks :: Int, doubles :: Int, xoverfs :: Int,
                         brackets :: Int, stepsAndJumps :: Int,
                         lastStep :: Maybe Arrow, lastJack :: Maybe Arrow,
                         lastFlip :: Bool, lastFoot :: Foot, stepsLR :: [Bool] }

commitStream :: AnalysisState -> AnalysisState
commitStream s = maybe s0 splitStream splitIndex
    where -- number of steps in the chunk of stream
          ns = length $ stepsLR s
          -- number of (seemingly) crossed-over steps
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
analyzeStep s (J _) = -- TODO change to Other only and handle bracketables below
    -- a jump resets the footing, so the next step can be stepped with either
    -- foot. commit the stream so far to treat it separately from what follows.
    -- bracket-jumps are, of course, future work.
    -- NB. old version! still holds for LR and DU jumps, as well as 3+s
    (commitStream s) { lastStep = Nothing, lastJack = Nothing,
                       stepsAndJumps = stepsAndJumps s + 1 }
-- analyzeStep s (J jump) = undefined -- TODO
analyzeStep s (A arrow)
    -- two steps on the same arrow might be a jack, or might be a footswitch.
    -- to figure out which, commit the stream so far, and begin a new stream
    -- whose footing will retroactively determine how to foot this step.
    -- also, unlike jumps, this step gets counted as part of the next stream.
    | lastStep s == Just arrow = stream (commitStream s) { lastJack = Just arrow }
    -- a normal streamy step.
    | otherwise = stream s
    where foot = alternate $ lastFoot s
          -- record whether we stepped on a matching or crossed-over L/R arrow.
          addStep ft L steps = steps ++ [ft == LeftFoot]
          addStep ft R steps = steps ++ [ft == RightFoot]
          addStep ft _ steps = steps -- U/D don't help to determine L/R footing.
          stream s = s { steps = steps s + 1, lastStep = Just arrow, lastFoot = foot,
                         stepsLR = addStep foot arrow $ stepsLR s,
                         stepsAndJumps = stepsAndJumps s + 1 }

-- the above function flips step before comparing,
-- so this starts the chart on the left foot, actually
analyze :: [Step] -> AnalysisState
analyze = commitStream . foldl analyzeStep (S 0 0 0 0 0 0 0 0 Nothing Nothing False LeftFoot [])

-- turns a line of stepchart, eg "0100", into a Step, e.g. "D"
-- in stepchart-ese: 1 = tap; 2 = hold; 3 = hold release; 4 = roll; M = mine
stepify :: String -> Maybe Step
stepify = step . map fst . filter snd . zip [L,D,U,R] . map (flip elem "124")
    where step [] = Nothing
          step [arrow] = Just $ A arrow
          step arrows = Just $ J $ toJump $ sort arrows -- ensure LDUR order, but
          -- TODO i *think* the sort is not necessary, so after implemented,
          -- TODO try removing it and testing to see if same resulce?
          toJump [L,D] = LD
          toJump [L,U] = LU
          toJump [D,R] = DR
          toJump [U,R] = UR
          toJump [L,R] = Other -- cannot bracket opposite-arrow jumps
          toJump [D,U] = Other -- (perhaps aka, "candle jumps"???)
          toJump (_:_:_:_) = Other -- any other jumps must be 3+

-- handles chart metadata and formats the analysis result into tsv
process :: (String, [String]) -> String
process (title,(name:diff:feet:_:rest)) = intercalate "\t" $ metadata ++ result
    where -- chop off the trailing ":"
          trim = reverse . tail . dropWhile isSpace . reverse . dropWhile isSpace
          metadata = map trim [title,name,diff,feet]
          finalState = analyze $ catMaybes $ map (stepify . dropWhile isSpace) rest
          -- printing stepsandjumps first for backwards compatibility with the
          -- 2017-era test cases, which expect just 'steps' (== streamy steps)
          -- (their answers must be a uninterrupted substring of the output)
          result = map (show . ($ finalState)) [stepsAndJumps, steps, xovers, switches,
                                                jacks, doubles, xoverfs, brackets]

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
