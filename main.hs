import Control.Monad
import Data.IORef        
import qualified Data.Map as Map
import System.Exit
import System.Environment
import System.IO.Unsafe

import Text.Parsec

import Expr
import Parse
import Eval
import ExprTypeCheck

readExpr :: String -> Expr
readExpr input = case parse parseExprs "hslisp" input of
    Left err -> LispSymbol $ "\nNo match: " ++ show err
    Right val -> val

re :: String -> Context -> (Context, Expr) -- Read, Eval
re str ctx = eval ctx (readExpr str)

repl :: IO() -- Read, Eval, Print, Loop
repl = do
            putStrLn "--- Haskell Lisp REPL ---"
            ctxRef <- newIORef Map.empty
            let loop = do
                putStr "hslisp> "
                line <- getLine
                when (line == "quit") exitSuccess
                ctx <- readIORef ctxRef
                let val = re line ctx
                print $ snd val
                writeIORef ctxRef (fst val)
                loop     
            loop

main :: IO()
main = do args <- getArgs
          case args of
              [filename] -> do content <- readFile filename
                               let val = re content Map.empty
                               when (isErrorExpr $ snd val) (print $ snd val)
                               return ()
              [] -> repl