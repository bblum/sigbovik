-- The randomly-scoped lambda calculus.
-- Ben Blum <bblum@cs.cmu.edu>

module Main
where

-- import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Control.Monad (when)
import Control.Applicative ((<$>))
import System.Random
import Data.List ((!!))
import Debug.Trace
-- import Data.Maybe

data Expr = Var String [Expr]
          | App Expr Expr
          | Lam String Expr
          | Zero
          | Suc Expr
          | Natrec Expr Expr String Expr
          | Y1 Expr
          | Y2 Expr

instance Show Expr where
    show (Var name es) = name ++ show es
    show (App e1 e2) = show e1 ++ " " ++ show e2
    show (Lam name e) = "(\\" ++ name ++ "." ++ show e ++ ")"
    show (Zero) = "0"
    show (Suc e) =
        let isnat Zero = True
            isnat (Suc e) = isnat e
            isnat _ = False
            tonat Zero = 0
            tonat (Suc e) = 1 + tonat e
            tonat _ = error "shit"
        in if isnat e then show $ tonat $ Suc e
           else "S(" ++ show e ++ ")"
    show (Natrec e e1 name e2) =
        "(natrec " ++ show e ++ " 0 => " ++ show e1 ++
        "| S(" ++ name ++ ") => " ++ show e2 ++ ")"
    show (Y1 e) = "Y1 " ++ show e
    show (Y2 e) = "Y2 " ++ show e

----

-- Called when capturing under another lambda.
subst_shadow :: Expr -> String -> Expr -> Expr
subst_shadow e0 name (Var name2 es) =
    if name == name2 then
        -- trace ("Adding " ++ show e0 ++ " to " ++ name ++ show es) $
        Var name2 (e0:es) -- XXX: need to map here, as below?
    else Var name2 es -- $ map (subst_shadow e0 name) es
subst_shadow e0 name (App e1 e2) = App (subst_shadow e0 name e1) (subst_shadow e0 name e2)
subst_shadow e0 name (Lam name2 e) = Lam name2 $ subst_shadow e0 name e -- capture-seeking substitution
subst_shadow e0 name Zero = Zero
subst_shadow e0 name (Suc e) = Suc $ subst_shadow e0 name e
subst_shadow e0 name (Natrec e e1 name2 e2) =
    -- natrec [e0/name]e: 0 => [e0/name]e1 | S(name2) => [e0/name]e2
    Natrec (subst_shadow e0 name e) (subst_shadow e0 name e1) name2 (subst_shadow e0 name e2) -- capture-seeking, again
subst_shadow e0 name (Y1 e) = Y1 $ subst_shadow e0 name e
subst_shadow e0 name (Y2 e) = Y2 $ subst_shadow e0 name e


subst_pull :: Expr -> String -> Expr -> IO Expr
subst_pull e0 name (Var name2 es) =
    if name == name2 then
        let list = e0:es
        in do i <- getStdRandom $ randomR (0, length list - 1)
              return $ list !! i
    else return $ Var name2 es
subst_pull e0 name (App e1 e2) =
    do e1' <- subst_pull e0 name e1
       e2' <- subst_pull e0 name e2
       return $ App e1' e2'
subst_pull e0 name (Lam name2 e) =
    if name == name2 then
        return $ Lam name2 $ subst_shadow e0 name e -- capture-seeking substitution
    else
        Lam name2 <$> subst_pull e0 name e
subst_pull e0 name Zero = return Zero
subst_pull e0 name (Suc e) = Suc <$> subst_pull e0 name e
subst_pull e0 name (Natrec e e1 name2 e2) =
    do e'  <- subst_pull e0 name e
       e1' <- subst_pull e0 name e1
       e2' <- if name == name2 then
                  return $ subst_shadow e0 name e2 -- capture-seeking, again
              else
                  subst_pull e0 name e2
       return $ Natrec e' e1' name2 e2'
subst_pull e0 name (Y1 e) = Y1 <$> subst_pull e0 name e
subst_pull e0 name (Y2 e) = Y2 <$> subst_pull e0 name e

eval :: Expr -> IO Expr
eval (Var name es) =
    error "can't evaluate var"
    -- do i <- getStdRandom $ randomR (0, length es - 1)
    --    -- when (length es > 1) $
    --    --     putStrLn $ "Choosing " ++ show (es!!i) ++ " for " ++ name ++ " from " ++ show es
    --    eval $ es !! i
eval (App (Y1 e1) e2) = eval $ e1 $$ e2 $$ Y1 e1
eval (App (App (Y2 e1) e2) e3) = eval $ e1 $$ e2 $$ e3 $$ Y2 e1
eval (App e1 e2) =
    do e1' <- eval e1
       case e1' of
           Lam name e1'' -> do e1''' <- subst_pull e2 name e1''; eval e1'''
           _ -> error ("fuck" ++ show (App e1' e2))
eval (Lam name e) = return $ Lam name e
eval Zero = return Zero
eval (Suc e) = Suc <$> eval e
eval (Natrec e e1 name e2) =
    do e' <- eval e
       case e' of Zero -> eval e1
                  Suc e'' -> do e2' <- subst_pull e'' name e2; eval e2'
                  _ -> error $ "crap" ++ show e'
eval (Y1 e) = error "eval Y1??" -- return $ Y2 e
eval (Y2 e) = error "eval Y2??" -- return $ Y2 e

----

var name = Var name []
e1 $$ e2 = App e1 e2

{-
ycomb = let f = "ycomb_f"
            x1 = "ycomb_x1"
            x2 = "ycomb_x2"
            v = "ycomb_v"
        in Lam f $ 
               (Lam x1 $ var f $$ (var x1 $$ var x1)) $$
               (Lam x2 $ var f $$ (var x2 $$ var x2))

ycomb_rntz = -- (\x.xx)(\ygab.gab(yyg)) -- doesn't quite work; a still captured
    let x = "rntz_x"
        y = "rntz_y"
        g = "rntz_g"
        a = "rntz_a"
        b = "rntz_b"
    in (Lam x $ var x $$ var x) $$
       (Lam y $ Lam g $ Lam a $ Lam b $
        var g $$ var a $$ var b $$ (var y $$ var y $$ var g))
-}

-- another attempt at the above: (\x.xx)(\yg. (\1234.1(234)) (\z.z(yyg)) (\ab.gab))
-- still doesn't work because 'z' is captured inside yyg.

-- another attempt [\gab. (\x.xx)(\y. gab( ((\inner.\gab.inner inner)y) g )]
-- this one's even broken in the regular lambda calc. -_-

-- Finally, the correct attempt. The calling convention itself needs to change.
-- The one-argument combinator is \a.\g.gag. For two args, \a.\b.\g.gabg.
-- Functions must also contain it, -- to call it again (because it can't see
-- itself), and pass it the recursive argument first before itself. For example:
-- add = \xy. Y2 x y (\mn. case m of 0 => \f. n | S(m2) => \f. S(Y2 m2 n f)
y1 name =
    let g = "y2_g_" ++ name
        a = "y2_a_" ++ name
    in Lam a $ Lam g $ var g $$ var a $$ var g
y2 name =
    let g = "y2_g_" ++ name
        a = "y2_a_" ++ name
        b = "y2_b_" ++ name
    in Lam a $ Lam b $ Lam g $ var g $$ var a $$ var b $$ var g

true = Lam "church_t" $ Lam "church_t2" $ var "church_t"
false = Lam "church_f" $ Lam "church_f2" $ var "church_f2"

truefalse = Lam "church_tf" $ Lam "church_tf" $ var "church_tf" -- true with p=0.5

true3false = truefalse $$ true $$ truefalse  -- true with p=0.75
truefalse3 = truefalse $$ false $$ truefalse -- true with p=0.25

randombit = truefalse $$ Zero $$ Suc Zero

-- Y(\f. \n. truefalse n (f (succ n))) zero

randomnat =
    let f = "randomnat_f"
        n = "randomnat_n"
        r = Lam n $ Lam f $ truefalse $$ var n $$ (y1 "randomnat" $$ (Suc $ var n) $$ var f)
    in y1 "randomnat" $$ Zero $$ r

rand =
    let x = "r_x" -- this is the one that gets "captured"
        myself = "r_myself"
        thing = "r_thing"
        n = "r_n"
        n2 = "r_n2"
        arg = "r_arg"
        r = Lam thing $ Lam n $ -- Lam myself $
                Natrec (var n) (Lam myself $ var thing $$ Zero) -- 0 => thing 0
                       n2 (Lam myself $ y2 "rand" $$ ((Lam x $ var thing) $$ var n) $$ var n2 $$ var myself)
    in Lam arg $ y2 "rand" $$ (Lam x $ var x) $$ var arg $$ r

tonat 0 = Zero
tonat n = if n > 0 then Suc $ tonat $ n-1 else error "?????"

--

-- Deterministic programming.

-- Works!
add = -- \mn. case m of 0 => n | S(m2) => S(add m2 n)
    let n = "add_n"
        m = "add_m"
        m2 = "add_m2"
        myself = "add_myself"
        arg1 = "add_arg1"
        arg2 = "add_arg2"
        add_inner =
            Lam m $ Lam n $
               Natrec (var m) (Lam myself $ var n)
                      m2 (Lam myself $ Suc $ y2 "add" $$ var m2 $$ var n $$ var myself)
    in Lam arg1 $ Lam arg2 $ y2 "add" $$ var arg1 $$ var arg2 $$ add_inner

-- XXX: Something about this is currently broken.
times = -- \mn. case m of 0 => 0 | s(m2) => add n (times m2 n)
    let n = "times_n"
        m = "times_m"
        m2 = "times_m2"
        myself = "times_myself"
        arg1 = "times_arg1"
        arg2 = "times_arg2"
        times_inner =
            Lam m $ Lam n $
                Natrec (var m) (Lam myself $ Zero)
                       m2 (Lam myself $ add $$ var n $$ (y2 "times" $$ var m2 $$ var n $$ var myself))
    in Lam arg1 $ Lam arg2 $ y2 "times" $$ var arg1 $$ var arg2 $$ times_inner

-- Broken even worse than times. Probably same problem.
fact = -- \n. case n of 0 => 1 | s(n2) => times n (fact n2)
    let n = "fact_n"
        n2 = "fact_n2"
        myself = "fact_myself"
        arg = "fact_arg"
        fact_inner =
            Lam n $ Natrec (var n) (Lam myself $ Suc Zero)
                           n2 (Lam myself $ times $$ var n $$ (y1 "fact" $$ var n2 $$ var myself))
    in Lam arg $ y1 "fact" $$ var arg $$ fact_inner

-- Testing

terms =
    map (\list -> map (\(n,m) -> add $$ tonat n $$ tonat m) list) $ [
        [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),(0,7),(0,8),(0,9)],
        [(1,0),(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9)],
        [(2,0),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9)],
        [(3,0),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)],
        [(4,0),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9)],
        [(5,0),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9)],
        [(6,0),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9)],
        [(7,0),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9)],
        [(8,0),(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9)],
        [(9,0),(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]]


main :: IO ()
main = do terms' <- sequence $ map (mapM eval) terms; mapM_ (putStrLn . show) terms'
