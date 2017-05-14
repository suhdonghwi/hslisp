import Control.Monad
import Data.IORef        
import qualified Data.Map as Map
import System.Exit
import System.Environment
import qualified Control.Exception as Exc

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

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

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
            --   [filename] -> do content <- readFile filename
            --                    --  let splitedContent = wordsWhen (== '\n') content 
            --                    Exc.catch (return $ re content Map.empty) handler
            --                    -- _ <- return $ map (`re` Map.empty) splitedContent
            --                    return ()
            --                 where handler :: Exc.ErrorCall -> IO()
            --                       handler _ = putStrLn "what the fuck"
              [] -> repl