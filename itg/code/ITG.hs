import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple

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
                         lastFlip :: Bool, lastFoot :: Foot, stepsLR :: [Bool],
                         -- used for bracket jumps e.g. if left foot's last
                         -- step was on U, right foot cannot bracket UR
                         lastArrowLR :: ([Arrow], [Arrow]), -- (left, right)
                         lastTrueLastArrowLR :: ([Arrow], [Arrow]) -- ugh...
                       }

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
          -- this nonsense merges the lastArrow lists with a known good state
          -- snapshotted from the previous stream commit, because a 1-step long
          -- stream e.g. may end up with an empty list on the other foot, and
          -- you want neither to clobber the old value with [], or clobber it by
          -- writing willy nilly while you're still not sure if you'll flip.
          -- analyze doesn't need to worry about this; the correct thing will
          -- show up in lastArrowLR each time regardless.
          unifySideArrows []     old = old
          unifySideArrows recent old = recent
          unifyArrows (ls,rs) = (unifySideArrows ls $ fst $ lastTrueLastArrowLR s,
                                 unifySideArrows rs $ snd $ lastTrueLastArrowLR s)
          -- update the true lastFoot and lastArrows (if nonzero stream)
          -- this way the value always shows our true position on the pad after
          -- the stream ended, to see if bracket jumps are possible
          -- TODO: maintain a lastTrueLastArrowLR, snapshotted at time of last jump
          -- merge it in with this in case either list is empty on this thing
          -- patterns such as e.g., LD(bracket) R(step) LD(bracket) R(step)
          -- may possibly trigger it
          (trueLastFoot, trueLastArrowLR) =
              -- no stream, or not flipped: keep as is
              if ns == 0 || not f then (lastFoot s, unifyArrows $ lastArrowLR s)
              -- notes existed & was flipped; flip these
              else (alternate $ lastFoot s, unifyArrows $ swap $ lastArrowLR s)
          s0 = s { xovers   = xovers   s + if f then ns - nx else nx,
                   switches = switches s + fromEnum (f == lastFlip s && jack),
                   jacks    = jacks    s + fromEnum (f /= lastFlip s && jack),
                   xoverfs  = xoverfs  s + fromEnum (f == lastFlip s && jackLR),
                   lastFoot = trueLastFoot, lastArrowLR = trueLastArrowLR,
                   lastTrueLastArrowLR = trueLastArrowLR, -- update snapshot
                   lastFlip = f, stepsLR = [] }

-- a jump resets the footing, so the next step can be stepped with either
-- foot. commit the stream so far to treat it separately from what follows.
-- this is the old version; still holds for LR and DU jumps, as well as 3+s.
-- we clear lastArrowLR here to represent resetting footing
-- TODO: add a J UD case and have them track lastfoot to maintain the
-- player's facing through UD jumps (J LR not relevant unless attempting
-- xover bracket jumps)
-- ex test cases. should bracket:
-- L R U UD LU
-- should not bracket:
-- L R U UD LD
commitJump s = s { lastStep = Nothing, lastJack = Nothing,
                   stepsAndJumps = stepsAndJumps s + 1, lastArrowLR = ([],[]) }

analyzeStep :: AnalysisState -> Step -> AnalysisState
analyzeStep s (J Other) =
    commitJump $ commitStream s
analyzeStep s (J jump) =
    if can_bracket jump newLastFoot lastArrows then
        (commitJump s') { brackets = brackets s' + 1,
                          lastFoot = newLastFoot,
                          lastArrowLR = updateLastArrow newLastFoot lastArrows $ bracketArrows jump
                        }
    else
        commitJump s' -- jump it normally
    -- TODO: instruct commitstream to breka ties to bias towards bracketing this
    -- TODO-2: do a "check" for bracket on subsequent stream where you fix the
    -- foot and test if it flips, and if it needs to flip, you cancel the bracket
    -- ...either that, or you actually FORCE it never to flip
    -- idk which would be more natural!
    where s' = commitStream s
          newLastFoot = alternate $ lastFoot s'
          lastArrows = lastArrowLR s'
          bracketArrows LD = [L,D]
          bracketArrows LU = [L,U]
          bracketArrows DR = [D,R]
          bracketArrows UR = [U,R]
          -- test if e.g. right foot is on U, then left foot cannot bracket LU
          interferes footArrows j = any (\a -> elem a footArrows) $ bracketArrows j
          -- XXX: this will lead to artifacts where very first step of song
          -- can or cannot be bracketed... depending on what foot the state is
          -- initialized with... i'm not sure this matters enough to bother
          -- note 2nd argument is the foot to attempt to bracket with
          can_bracket LD LeftFoot (_, lastRight) = not $ interferes lastRight LD
          can_bracket LU LeftFoot (_, lastRight) = not $ interferes lastRight LU --(elem L lastRight) && not (elem U lastRight)
          can_bracket DR RightFoot (lastLeft, _) = not $ interferes lastLeft DR
          can_bracket UR RightFoot (lastLeft, _) = not $ interferes lastLeft UR
          can_bracket _ _ _ = False -- don't allow jack-brackets (future work?)
          -- again the foot is the foot we bracket with
          updateLastArrow LeftFoot  (lastLeft,lastRight) arrows = (arrows,lastRight)
          updateLastArrow RightFoot (lastLeft,lastRight) arrows = (lastLeft,arrows)
          
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
          -- remember where this foot ends up (U/D important here for brackets)
          -- XXX: is this wrong? overwriting a fixed (l,r)
          -- with something that could be flipped later
          rememberFoot LeftFoot  arrow (_,r) = ([arrow],r)
          rememberFoot RightFoot arrow (l,r) = (l,[arrow])
          stream s = s { steps = steps s + 1, lastStep = Just arrow, lastFoot = foot,
                         stepsLR = addStep foot arrow $ stepsLR s,
                         stepsAndJumps = stepsAndJumps s + 1 }

-- the above function flips step before comparing,
-- so this starts the chart on the left foot, actually
analyze :: [Step] -> AnalysisState
analyze = commitStream . foldl analyzeStep (S 0 0 0 0 0 0 0 0 Nothing Nothing False LeftFoot [] ([],[]) ([],[]))

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
