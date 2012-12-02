-- The randomly-scoped lambda calculus.
-- Ben Blum <bblum@cs.cmu.edu>

module Main
where

-- import Data.IORef (IORef,newIORef,readIORef,writeIORef)
-- import Control.Monad
import Control.Applicative ((<$>))
import System.Random
import Data.List ((!!))
-- import Data.Maybe

data Expr = Var String [Expr]
          | App Expr Expr
          | Lam String Expr
          | Zero
          | Suc Expr

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

----

subst :: Expr -> String -> Expr -> Expr
subst e0 name (Var name2 es) =
    if name == name2 then Var name2 (e0:es)
    else Var name2 es
subst e0 name (App e1 e2) = App (subst e0 name e1) (subst e0 name e2)
subst e0 name (Lam name2 e) = Lam name2 $ subst e0 name e -- capture-seeking substitution
subst e0 name Zero = Zero
subst e0 name (Suc e) = Suc $ subst e0 name e

eval :: Expr -> IO Expr
eval (Var name es) =
    do i <- getStdRandom $ randomR (0, length es - 1)
       eval $ es !! i
eval (App e1 e2) =
    do e1' <- eval e1
       case e1' of
           Lam name e1'' -> eval $ subst e2 name e1''
           _ -> error ("fuck" ++ show (App e1' e2))
eval (Lam name e) = return $ Lam name e
eval Zero = return Zero
eval (Suc e) = Suc <$> eval e

----

var name = Var name []
e1 $$ e2 = App e1 e2

ycomb = let f = "ycomb_f"
            x = "ycomb_x"
        in Lam f $ 
               (Lam x $ var f $$ (var x $$ var x)) $$
               (Lam x $ var f $$ (var x $$ var x))

true = Lam "church_t" $ Lam "church_t2" $ var "church_t"
false = Lam "church_f" $ Lam "church_f2" $ var "church_f2"

truefalse = Lam "church_tf" $ Lam "church_tf" $ var "church_tf" -- true with p=0.5

true3false = truefalse $$ true $$ truefalse  -- true with p=0.75
truefalse3 = truefalse $$ false $$ truefalse -- true with p=0.25

randombit = truefalse $$ Zero $$ Suc Zero

-- Y(\f. \n. (\x.(\x.x))(n)(f (succ n)))(zero)
-- Y(\f. \n. truefalse n (f 

randomnat =
    let f = "randomnat_f"
        n = "randomnat_n"
        r = Lam f $ Lam n $ truefalse $$ var n $$ (var f $$ (Suc $ var n))
    in ycomb $$ r $$ Zero

terms = [
    -- randombit,
    randomnat]

main :: IO ()
main = do terms' <- mapM eval terms; mapM_ (putStrLn . show) terms'
