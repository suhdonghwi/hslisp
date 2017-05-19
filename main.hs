import Control.Monad
import Data.IORef        
import qualified Data.Map as Map
import System.Exit
import System.Environment
import System.IO

import Text.Parsec

import Expr
import Parse
import Eval
import ExprTypeCheck

readExpr :: String -> Expr
readExpr input = case parse parseExprs "hslisp" input of
    Left err -> LispError $ "\nNo match: " ++ show err
    Right val -> val

re :: String -> Context -> (Context, Expr) -- Read, Eval
re str ctx = eval ctx (readExpr str)

repl :: Context -> IO() -- Read, Eval, Print, Loop
repl initCtx = do
            ctxRef <- newIORef initCtx
            let loop = do
                putStr "λ> "
                hFlush stdout
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
              [] -> repl builtinMap
              filenames -> do
                        ctxRef <- newIORef builtinMap 
                        forM_ filenames $ \filename -> do
                            putStrLn $ "Loading " ++ filename ++ " ..."
                            content <- readFile filename
                            ctx <- readIORef ctxRef
                            let val = re content ctx
                            when (isErrorExpr $ snd val) (print (snd val) >> exitFailure)
                            writeIORef ctxRef (fst val)
                            putStrLn $ "Successfully loaded " ++ filename ++ "."
                        ctx <- readIORef ctxRef
                        repl ctx