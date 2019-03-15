import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple
import Debug.Trace

data Arrow = L| D | U | R deriving (Ord, Eq, Show)

data Jump = LD | LU | DR | UR | LR | DU | Other deriving (Ord, Eq, Show)

data Step = A Arrow | J Jump deriving (Ord, Eq, Show)

data Foot = LeftFoot | RightFoot deriving (Ord, Eq, Show)

alternate LeftFoot = RightFoot
alternate RightFoot = LeftFoot

-- TODO add a field to track what the last arrow the left foot and right foot were on
-- if you commitstream flipped, you need to flip those around too
data AnalysisState = S { steps :: Int, xovers :: Int, switches :: Int,
                         jacks :: Int, doubles :: Int, xoverfs :: Int,
                         brackets :: Int, stepsAndJumps :: Int,
                         lastStep :: Maybe Arrow, lastJack :: Maybe Arrow,
                         lastFlip :: Bool, lastFoot :: Maybe Foot,
                         justBracketed :: Bool, stepsLR :: [Bool],
                         -- used for bracket jumps e.g. if left foot's last
                         -- step was on U, right foot cannot bracket UR
                         lastArrowLR :: ([Arrow], [Arrow]), -- (left, right)
                         lastTrueLastArrowLR :: ([Arrow], [Arrow]) -- ugh...
                       } deriving Show

commitStream tiebreakFoot s = s' -- traceShow ("commit", s, "=>", s') s'
    where s' = commitStream' tiebreakFoot s

commitStream' :: Maybe Foot -> AnalysisState -> AnalysisState
commitStream' tiebreakFoot s = maybe s0 splitStream splitIndex
    where -- number of steps in the chunk of stream
          ns = length $ stepsLR s
          -- number of (seemingly) crossed-over steps
          nx = length $ filter not $ stepsLR s
          -- if more than half the L/R steps in this stream were crossed over,
          -- then we got the footing backwards and need to flip the stream.
          -- as tiebreaker #1, don't flip if immediately preceded by a bracket
          flipTieBreak _ | justBracketed s = False
          -- as tiebreaker #2, if the stream is followed by a bracketable jump,
          -- choose whichever flip-or-no-flip lets us bracket that jump.
          flipTieBreak (Just bracketFoot) = lastFoot s == Just bracketFoot
          -- as tiebreaker #3, flip if the chart is already more jacky than
          -- footswitchy, i.e., if past streams flipped more often than not.
          flipTieBreak Nothing = (switches s > jacks s) == lastFlip s
          f = nx * 2 > ns || nx * 2 == ns && flipTieBreak tiebreakFoot
          -- if "too much" of the stream is *completely* crossed-over, force
          -- a double-step there by splitting the stream to stay facing forward.
          -- heuristic value was chosen by inspection on Subluminal - After Hours.
          splitIndex = findIndex (isPrefixOf $ replicate 9 f) $ tails $ stepsLR s
          -- prevent infinite splittage if the fux section starts immediately;
          -- in that case split instead at fux's end (i.e., first non-crossed step).
          splitStream 0 = splitStream $ fromJust $ findIndex (/= f) $ stepsLR s
          splitStream i = s2 { doubles = doubles s2 + 1 }
              where (steps1, steps2) = splitAt i $ stepsLR s
                    s1 = commitStream Nothing      s  { stepsLR = steps1 }
                    s2 = commitStream tiebreakFoot s1 { stepsLR = steps2, lastJack = Nothing }
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
          (trueLastFoot, trueLastArrowLR) =
              -- was flipped; flip these (even if ns == 0, b/c that excludes U/D)
              if f then (alternate <$> lastFoot s, unifyArrows $ swap $ lastArrowLR s)
              -- not flipped: keep as is
              else (lastFoot s, unifyArrows $ lastArrowLR s)
          s0 = s { xovers   = xovers   s + if f then ns - nx else nx,
                   switches = switches s + fromEnum (f == lastFlip s && jack),
                   jacks    = jacks    s + fromEnum (f /= lastFlip s && jack),
                   xoverfs  = xoverfs  s + fromEnum (f == lastFlip s && jackLR),
                   lastFoot = trueLastFoot, lastArrowLR = trueLastArrowLR,
                   lastTrueLastArrowLR = trueLastArrowLR, -- update snapshot
                   justBracketed = False,
                   -- if we had to flip a stream right after a bracket jump,
                   -- that'd make it retroactively unbracketable; if so cancel it
                   brackets = brackets s - (if f && justBracketed s && ns > 0 then 1 else 0),
                   lastFlip = f, stepsLR = [] }

-- a jump resets the footing, so the next step can be stepped with either
-- foot. commit the stream so far to treat it separately from what follows.
-- this is the old version; still holds for LR and DU jumps, as well as 3+s.
-- we clear lastArrowLR here to represent resetting footing, and also clear the
-- lastFoot; if it's bracketable, that will be re-set afterwards down below.
commitJump s = s { lastStep = Nothing, lastJack = Nothing,
                   lastFoot = Nothing, justBracketed = False,
                   stepsAndJumps = stepsAndJumps s + 1,
                   lastArrowLR = ([],[]), lastTrueLastArrowLR = ([],[]) }

analyzeStep :: AnalysisState -> Step -> AnalysisState
analyzeStep s (J DU) =
    -- for DU jumps, try to mimimic the facing the player currently has
    -- e.g. if L D DU LU, the DU faces right (left on U) so the LU can bracket
    (commitJump s') { lastArrowLR = newLastArrows, lastTrueLastArrowLR = newLastArrows }
    where s' = commitStream Nothing s
          -- there's gotta be a cleaner way to do this?
          decideDUFacing' _ False False True = ([D],[U]) -- easy case
          decideDUFacing' True False False _ = ([D],[U]) -- easy case
          decideDUFacing' False _ True False = ([U],[D]) -- easy case
          decideDUFacing' False True _ False = ([U],[D]) -- easy case
          decideDUFacing' True False True False = ([],[]) -- D footswitch; allow anyth
          decideDUFacing' False True False True = ([],[]) -- U footswitch; allow anyth
          decideDUFacing' False False False False = ([],[]) -- no footing known
          decideDUFacing' _ _ _ _ = error "same foot on both D and U at once??"
          decideDUFacing a b c d = x -- traceShow ("deciding ud facing",a,b,c,d,x) x
              where x = decideDUFacing' a b c d
          newLastArrows = decideDUFacing (elem D $ fst $ lastArrowLR s') -- left on D?
                                         (elem U $ fst $ lastArrowLR s') -- or on U?
                                         (elem D $ snd $ lastArrowLR s') -- right on D?
                                         (elem U $ snd $ lastArrowLR s') -- or on U?
analyzeStep s (J jump) | elem jump [LR,Other] =
    let s'' = commitJump $ commitStream Nothing s in s'' -- traceShow ("other jump", s, s'') s''
analyzeStep s (J jump) =
    if can_bracket jump newLastFoot lastArrows then
        (commitJump s') { brackets = brackets s' + 1,
                          lastFoot = Just newLastFoot,
                          justBracketed = True, -- contingent on not next stream flip
                          lastArrowLR = ([],[]), -- newLastArrows,
                          lastTrueLastArrowLR = newLastArrows -- XXX: is this correct? seems to work without
                        }
    else
        let s'' = commitJump s' in s'' -- traceShow ("unbrackerable jump", s, s'') s'' -- jump it normally
    -- TODO: instruct commitstream to breka ties to bias towards bracketing this
    -- TODO-2: do a "check" for bracket on subsequent stream where you fix the
    -- foot and test if it flips, and if it needs to flip, you cancel the bracket
    -- ...either that, or you actually FORCE it never to flip
    -- idk which would be more natural!
    where s' = commitStream (Just $ defaultJumpFoot jump) s -- tell it how to break tie
          defaultJumpFoot LD = LeftFoot
          defaultJumpFoot LU = LeftFoot
          defaultJumpFoot DR = RightFoot
          defaultJumpFoot UR = RightFoot
          -- if foot-alternation is not established, e.g. we just did a LR jump,
          -- or it's the very start of the chart, err towards bracketing
          -- XXX: what is this doing here?
          -- XXX: the foot should either be the bracket foot, or Nothing at all
          newLastFoot = maybe (defaultJumpFoot jump) alternate $ lastFoot s'
          lastArrows = lastArrowLR s'
          bracketArrows LD = [L,D]
          bracketArrows LU = [L,U]
          bracketArrows DR = [D,R]
          bracketArrows UR = [U,R]
          -- test if e.g. right foot is on U, then left foot cannot bracket LU
          interferes footArrows j = any (\a -> elem a footArrows) $ bracketArrows j
          -- note 2nd argument is the foot to attempt to bracket with
          can_bracket' LD LeftFoot (_, lastRight) = not $ interferes lastRight LD
          can_bracket' LU LeftFoot (_, lastRight) = not $ interferes lastRight LU --(elem L lastRight) && not (elem U lastRight)
          can_bracket' DR RightFoot (lastLeft, _) = not $ interferes lastLeft DR
          can_bracket' UR RightFoot (lastLeft, _) = not $ interferes lastLeft UR
          can_bracket' _ _ _ = False -- don't allow jack-brackets (future work?)
          can_bracket j foot (ll,lr) =
              -- traceShow ("can bracker?",j,foot,ll,lr) $ can_bracket' j foot (ll,lr)
              can_bracket' j foot (ll,lr)
          -- again the foot is the foot we bracket with
          updateLastArrow LeftFoot  (lastLeft,lastRight) arrows = (arrows,lastRight)
          updateLastArrow RightFoot (lastLeft,lastRight) arrows = (lastLeft,arrows)
          -- compute new foot positions, persisting thru the commit-jump
          newLastArrows = updateLastArrow newLastFoot lastArrows $ bracketArrows jump
          
analyzeStep s (A arrow)
    -- two steps on the same arrow might be a jack, or might be a footswitch.
    -- to figure out which, commit the stream so far, and begin a new stream
    -- whose footing will retroactively determine how to foot this step.
    -- also, unlike jumps, this step gets counted as part of the next stream.
    | lastStep s == Just arrow = stream (commitStream Nothing s) { lastJack = Just arrow }
    -- a normal streamy step.
    | otherwise = stream s
    where foot = maybe LeftFoot alternate $ lastFoot s
          -- record whether we stepped on a matching or crossed-over L/R arrow.
          addStep ft L steps = steps ++ [ft == LeftFoot]
          addStep ft R steps = steps ++ [ft == RightFoot]
          addStep ft _ steps = steps -- U/D don't help to determine L/R footing.
          -- remember where this foot ends up (U/D important here for brackets)
          -- XXX: is this wrong? overwriting a fixed (l,r)
          -- with something that could be flipped later
          rememberFoot LeftFoot  arrow (_,r) = ([arrow],r)
          rememberFoot RightFoot arrow (l,r) = (l,[arrow])
          stream s = s { steps = steps s + 1, lastStep = Just arrow,
                         lastFoot = Just foot,
                         stepsLR = addStep foot arrow $ stepsLR s,
                         lastArrowLR = rememberFoot foot arrow $ lastArrowLR s,
                         stepsAndJumps = stepsAndJumps s + 1 }

analyze :: [Step] -> AnalysisState
analyze = commitStream Nothing . foldl analyzeStep (S 0 0 0 0 0 0 0 0 Nothing Nothing False Nothing False [] ([],[]) ([],[]))

-- turns a line of stepchart, eg "0100", into a Step, e.g. "D"
-- in stepchart-ese: 1 = tap; 2 = hold; 3 = hold release; 4 = roll; M = mine
stepify :: String -> Maybe Step
stepify = step . map fst . filter snd . zip [L,D,U,R] . map (flip elem "124")
    where step [] = Nothing
          step [arrow] = Just $ A arrow
          step arrows = Just $ J $ toJump arrows -- $ sort arrows -- sort not nec.
          toJump [L,D] = LD
          toJump [L,U] = LU
          toJump [D,R] = DR
          toJump [U,R] = UR
          toJump [L,R] = LR -- cannot bracket opposite-arrow jumps
          toJump [D,U] = DU -- (perhaps aka, "candle jumps"???)
          toJump [_,_] = error "somehow not sorted??"
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
