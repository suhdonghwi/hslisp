module Eval where

import Data.Map (Map)         
import qualified Data.Map as Map
import Data.List
import Data.Fixed
import Data.Maybe
import System.IO.Unsafe

import Expr
import Unpack
import ExprTypeCheck

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

lispNumAdd :: Context -> [Expr] -> (Context, Expr)
lispNumAdd ctx args 
    | isFloatExpr $ head eval_args = (ctx, LispFloat $ sum $ map unpackFloat eval_args)
    | isIntegerExpr $ head eval_args = (ctx, LispInteger $ sum $ map unpackInteger eval_args)
    | otherwise = (ctx, LispError "function '+' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispNumSub :: Context -> [Expr] -> (Context, Expr)
lispNumSub ctx args
    | isFloatExpr $ head eval_args = (ctx, LispFloat $ foldl1 (-) (map unpackFloat eval_args))
    | isIntegerExpr $ head eval_args = (ctx, LispInteger $ foldl1 (-) (map unpackInteger eval_args))
    | otherwise = (ctx, LispError "function '-' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispNumMul :: Context -> [Expr] -> (Context, Expr)
lispNumMul ctx args
    | isFloatExpr $ head eval_args = (ctx, LispFloat $ product $ map unpackFloat eval_args)
    | isIntegerExpr $ head eval_args = (ctx, LispInteger $ product $ map unpackInteger eval_args)
    | otherwise = (ctx, LispError "function '*' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispNumDiv :: Context -> [Expr] -> (Context, Expr)
lispNumDiv ctx args 
    | isFloatExpr $ head eval_args = (ctx, LispFloat $ foldl1 (/) (map unpackFloat eval_args))
    | isIntegerExpr $ head eval_args = (ctx, LispInteger $ foldl1 div (map unpackInteger eval_args))
    | otherwise = (ctx, LispError "function '/' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispNumMod :: Context -> [Expr] -> (Context, Expr)
lispNumMod ctx args
    | isFloatExpr $ head eval_args = (ctx, LispFloat $ foldl1 mod' (map unpackFloat eval_args))
    | isIntegerExpr $ head eval_args = (ctx, LispInteger $ foldl1 mod (map unpackInteger eval_args))
    | otherwise = (ctx, LispError "function '/' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispEq :: Context -> [Expr] -> (Context, Expr)
lispEq ctx args
    | isFloatExpr $ head eval_args = (ctx, LispBoolean $ allTheSame (map unpackFloat eval_args))
    | isIntegerExpr $ head eval_args = (ctx, LispBoolean $ allTheSame (map unpackInteger eval_args))
    | isBooleanExpr $ head eval_args = (ctx, LispBoolean $ allTheSame (map unpackBool eval_args))
    | otherwise = (ctx, LispError "function '=' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispNEq :: Context -> [Expr] -> (Context, Expr)
lispNEq ctx args 
    | isFloatExpr $ head eval_args = (ctx, LispBoolean $ not $ allTheSame (map unpackFloat eval_args))
    | isIntegerExpr $ head eval_args = (ctx, LispBoolean $ not $ allTheSame (map unpackInteger eval_args))
    | isBooleanExpr $ head eval_args = (ctx, LispBoolean $ not $ allTheSame (map unpackBool eval_args))
    | otherwise = (ctx, LispError "function '/=' is not defined for the type")
    where eval_args = map (snd . eval ctx) args

lispAnd :: Context -> [Expr] -> (Context, Expr)
lispAnd ctx args = (ctx, LispBoolean $ all unpackBool eval_args)
    where eval_args = map (snd . eval ctx) args

lispOr :: Context -> [Expr] -> (Context, Expr)
lispOr ctx args = (ctx, LispBoolean $ any unpackBool eval_args)
    where eval_args = map (snd . eval ctx) args

lispIf :: Context -> [Expr] -> (Context, Expr)
lispIf ctx args
    | argsNum /= 3 = (ctx, LispError $ "'if' expected 3 arguments, but got " ++ show argsNum)
    | otherwise =
        if unpackBool $ snd $ eval ctx (head eval_args) then eval ctx (eval_args !! 1) else eval ctx (eval_args !! 2)
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispDefine :: Context -> [Expr] -> (Context, Expr)
lispDefine ctx args 
    | Map.notMember strHeadArg ctx =
        (Map.insert strHeadArg (eval_args !! 1) ctx, eval_args !! 1)
    | otherwise = (ctx, LispError $ "symbol name already bound \"" ++ strHeadArg ++ "\"")
    where strHeadArg = show $ head args
          eval_args = map (snd . eval ctx) args

lispLambda :: Context -> [Expr] -> (Context, Expr)
lispLambda ctx args
    | argsNum /= 2 = (ctx, LispError $ "'lambda' expected 2 arguments, but got " ++ show argsNum)
    | otherwise = (ctx, LispFunction $ \ctx2 arglist ->
            let merge_map = Map.fromList $ Map.toList ctx2 ++ zip (map unpackSymbol (unpackList (head args))) arglist 
            in (ctx2, snd $ eval merge_map (args !! 1)))
    where argsNum = length args

lispDefun :: Context -> [Expr] -> (Context, Expr)
lispDefun ctx args
    | argsNum /= 3 = (ctx, LispError $ "'defun' expected 3 arguments, but got " ++ show argsNum)
    | otherwise = lispDefine ctx [head args, snd $ lispLambda ctx (tail args)]
    where argsNum = length args

lispHead :: Context -> [Expr] -> (Context, Expr)
lispHead ctx args
    | argsNum /= 1 = (ctx, LispError $ "'head' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, snd $ eval ctx (head $ unpackConsList (snd $ eval ctx (head args))))
    where argsNum = length args

lispTail :: Context -> [Expr] -> (Context, Expr)
lispTail ctx args
    | argsNum /= 1 = (ctx, LispError $ "'tail' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, snd $ eval ctx (LispConsList $ tail $ unpackConsList (snd $ eval ctx (head args))))
    where argsNum = length args

lispPrepend :: Context -> [Expr] -> (Context, Expr)
lispPrepend ctx args
    | argsNum /= 2 = (ctx, LispError $ "':' expected 2 arguemnts, but got " ++ show argsNum)
    | otherwise = (ctx, LispConsList $ unpackConsList (head eval_args) ++ unpackConsList (eval_args !! 1))
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispListLength :: Context -> [Expr] -> (Context, Expr)
lispListLength ctx args 
    | argsNum /= 1 = (ctx, LispError $ "'length' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, LispInteger $ genericLength $ unpackConsList (head eval_args))
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispQuote :: Context -> [Expr] -> (Context, Expr)
lispQuote ctx args
    | argsNum /= 1 = (ctx, LispError $ "'quote' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, head args)
    where argsNum = length args

lispEval :: Context -> [Expr] -> (Context, Expr)
lispEval ctx args
    | argsNum /= 1 = (ctx, LispError $ "'eval' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = eval ctx (unpackList (head args) !! 1)
    where argsNum = length args

lispPrint :: Context -> [Expr] -> (Context, Expr)
lispPrint ctx args
    | argsNum /= 1 = (ctx, LispError $ "'print' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = unsafePerformIO $ do print $ head eval_args -- Ok, I agree. Sorry about that.
                                       return (ctx, head eval_args)
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispPutStr :: Context -> [Expr] -> (Context, Expr)
lispPutStr ctx args
    | argsNum /= 1 = (ctx, LispError $ "'putStr' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = unsafePerformIO $ do putStr $ map unpackChar (unpackConsList $ head eval_args)
                                       return (ctx, head eval_args)
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispPutStrLn :: Context -> [Expr] -> (Context, Expr)
lispPutStrLn ctx args
    | argsNum /= 1 = (ctx, LispError $ "'putStrLn' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = unsafePerformIO $ do putStrLn $ map unpackChar (unpackConsList $ head eval_args)
                                       return (ctx, head eval_args)
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

builtInMap :: Map String (Context -> [Expr] -> (Context, Expr))
builtInMap = Map.fromList [("+", lispNumAdd), 
                           ("-", lispNumSub), 
                           ("*", lispNumMul), 
                           ("/", lispNumDiv), 
                           ("%", lispNumMod),
                           ("=", lispEq), 
                           ("/=", lispNEq),
                           ("and", lispAnd), 
                           ("or", lispOr),
                           ("if", lispIf), 
                           ("define", lispDefine), 
                           ("lambda", lispLambda),
                           ("defun", lispDefun),
                           ("head", lispHead),
                           ("tail", lispTail),
                           ("++", lispPrepend),
                           ("length", lispListLength),
                           ("quote", lispQuote),
                           ("eval", lispEval),
                           ("print", lispPrint),
                           ("putStr", lispPutStr),
                           ("putStrLn", lispPutStrLn)]

eval :: Context -> Expr -> (Context, Expr)
eval ctx val@(LispFloat _) = (ctx, val)
eval ctx val@(LispInteger _) = (ctx, val)
eval ctx val@(LispBoolean _) = (ctx, val)
eval ctx val@(LispChar _) = (ctx, val)
eval ctx val@(LispFunction _) = (ctx, val)
eval ctx val@(LispSymbol symbol) = (ctx, fromMaybe (LispError $ "unbound symbol '" ++ show val ++ "'") (Map.lookup symbol ctx))

eval ctx (LispConsList lst) = (ctx, LispConsList $ map (snd . eval ctx) lst)
eval ctx (LispRangeList begin end) 
    | isFloatExpr eval_begin = (ctx, LispConsList (map LispFloat [(unpackFloat eval_begin) .. (unpackFloat eval_end)]))    
    | isIntegerExpr eval_begin = (ctx, LispConsList (map LispInteger [(unpackInteger eval_begin) .. (unpackInteger eval_end)]))
    | isCharExpr eval_begin = (ctx, LispConsList (map LispChar [(unpackChar eval_begin) .. (unpackChar eval_end)]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin
          eval_end = snd $ eval ctx end

eval ctx (LispRangeList2 begin begin2 end) 
    | isFloatExpr eval_begin = (ctx, LispConsList (map LispFloat [(unpackFloat eval_begin), (unpackFloat eval_begin2) .. (unpackFloat eval_end)]))    
    | isIntegerExpr eval_begin = (ctx, LispConsList (map LispInteger [(unpackInteger eval_begin), (unpackInteger eval_begin2) .. (unpackInteger eval_end)]))
    | isCharExpr eval_begin = (ctx, LispConsList (map LispChar [(unpackChar eval_begin), (unpackChar eval_begin2) .. (unpackChar eval_end)]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin
          eval_begin2 = snd $ eval ctx begin2
          eval_end = snd $ eval ctx end

eval ctx (LispList (LispSymbol func : args)) = case Map.lookup func builtInMap of 
                                                    Nothing -> (unpackFunc $ snd $ eval ctx (LispSymbol func)) ctx (map (snd . eval ctx) args)
                                                    Just lispfunc -> lispfunc ctx args
eval ctx (LispList (LispList func : args)) = (unpackFunc $ snd $ eval ctx (LispList func)) ctx args

eval ctx (LispDo [expr]) = eval ctx expr
eval ctx (LispDo (expr : exprs)) = let ctx2 = fst $ eval ctx expr
                                   in eval ctx2 (LispDo exprs)               

eval ctx _ = (ctx, LispError "Undefined eval function")