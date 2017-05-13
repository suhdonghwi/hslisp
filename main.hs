import System.IO 
import Control.Monad
import qualified Data.Map as Map
import Data.IORef

import Text.Parsec

import Expr
import Parse
import Eval

readExpr :: String -> Expr
readExpr input = case parse parseExpr "hslisp" input of
    Left err -> LispSymbol $ "No match: " ++ show err
    Right val -> val

re :: String -> Context -> (Context, Expr) -- Read, Eval
re str ctx = eval ctx (readExpr str)

repl :: IO() -- Read, Eval, Print, Loop
repl = do
            ctxRef <- newIORef Map.empty
            let loop = do
                putStr "hslisp> "
                line <- getLine
                ctx <- readIORef ctxRef
                print $ snd $ re line ctx
                writeIORef ctxRef (fst $ re line ctx)

                when (line /= "quit") loop     
            loop

main :: IO()
main = repl