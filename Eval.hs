module Eval where

import Data.Map (Map)         
import qualified Data.Map as Map
import Data.List

import Expr
import Unpack

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

lispNumAdd :: Context -> [Expr] -> (Context, Expr)
lispNumAdd ctx args = (ctx, LispInteger $ sum $ map unpackInteger eval_args)
    where eval_args = map (snd . eval ctx) args

lispStrConcat :: Context -> [Expr] -> (Context, Expr)
lispStrConcat ctx args = (ctx, LispString $ concatMap unpackString eval_args)
    where eval_args = map (snd . eval ctx) args

lispNumSub :: Context -> [Expr] -> (Context, Expr)
lispNumSub ctx args = (ctx, LispInteger $ foldl1 (-) (map unpackInteger eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumMul :: Context -> [Expr] -> (Context, Expr)
lispNumMul ctx args = (ctx, LispInteger $ product $ map unpackInteger eval_args)
    where eval_args = map (snd . eval ctx) args

lispNumDiv :: Context -> [Expr] -> (Context, Expr)
lispNumDiv ctx args = (ctx, LispInteger $ foldl1 div (map unpackInteger eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumMod :: Context -> [Expr] -> (Context, Expr)
lispNumMod ctx args = (ctx, LispInteger $ foldl1 mod (map unpackInteger eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumEq :: Context -> [Expr] -> (Context, Expr)
lispNumEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackInteger eval_args))
    where eval_args = map (snd . eval ctx) args

lispBoolEq :: Context -> [Expr] -> (Context, Expr)
lispBoolEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackBool eval_args))
    where eval_args = map (snd . eval ctx) args

lispStrEq :: Context -> [Expr] -> (Context, Expr)
lispStrEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackString eval_args))
    where eval_args = map (snd . eval ctx) args 

lispNumNEq :: Context -> [Expr] -> (Context, Expr)
lispNumNEq ctx args = (ctx, LispBoolean $ not $ allTheSame (map unpackInteger eval_args))
    where eval_args = map (snd . eval ctx) args

lispBoolNEq :: Context -> [Expr] -> (Context, Expr)
lispBoolNEq ctx args = (ctx, LispBoolean $ not $ allTheSame (map unpackBool eval_args))
    where eval_args = map (snd . eval ctx) args

lispStrNEq :: Context -> [Expr] -> (Context, Expr)
lispStrNEq ctx args = (ctx, LispBoolean $ not $ allTheSame (map unpackString eval_args))
    where eval_args = map (snd . eval ctx) args

lispAnd :: Context -> [Expr] -> (Context, Expr)
lispAnd ctx args = (ctx, LispBoolean $ all unpackBool eval_args)
    where eval_args = map (snd . eval ctx) args

lispOr :: Context -> [Expr] -> (Context, Expr)
lispOr ctx args = (ctx, LispBoolean $ any unpackBool eval_args)
    where eval_args = map (snd . eval ctx) args

lispIf :: Context -> [Expr] -> (Context, Expr)
lispIf ctx args
    | argsNum /= 3 = error $ "'if' expected 3 arguments, but got " ++ show argsNum
    | otherwise =
        if unpackBool $ snd $ eval ctx (head eval_args) then eval ctx (eval_args !! 1) else eval ctx (eval_args !! 2)
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispDefine :: Context -> [Expr] -> (Context, Expr)
lispDefine ctx args 
    | Map.notMember strHeadArg ctx =
        (Map.insert strHeadArg (eval_args !! 1) ctx, (eval_args !! 1))
    | otherwise = error $ "symbol name already bound \"" ++ strHeadArg ++ "\""
    where strHeadArg = (show $ head args)
          eval_args = map (snd . eval ctx) args

lispLambda :: Context -> [Expr] -> (Context, Expr)
lispLambda ctx args
    | argsNum /= 2 = error $ "'lambda' expected 2 arguments, but got " ++ show argsNum
    | otherwise = (ctx, LispFunction $ \ctx2 arglist ->
            let merge_map = Map.fromList $ Map.toList ctx2 ++ zip (map unpackSymbol (unpackList (head args))) arglist 
            in (ctx2, snd $ eval merge_map (args !! 1)))
    where argsNum = length args

lispDefun :: Context -> [Expr] -> (Context, Expr)
lispDefun ctx args
    | argsNum /= 3 = error $ "'defun' expected 3 arguments, but got " ++ show argsNum
    | otherwise = lispDefine ctx [(head args), (snd $ lispLambda ctx (tail args))]
    where argsNum = length args

lispHead :: Context -> [Expr] -> (Context, Expr)
lispHead ctx args
    | argsNum /= 1 = error $ "'head' expected 1 arguemnt, but got " ++ show argsNum
    | otherwise = (ctx, snd $ eval ctx (head $ unpackConsList (snd $ eval ctx (head args))))
    where argsNum = length args

lispTail :: Context -> [Expr] -> (Context, Expr)
lispTail ctx args
    | argsNum /= 1 = error $ "'tail' expected 1 arguemnt, but got " ++ show argsNum
    | otherwise = (ctx, snd $ eval ctx (LispConsList $ tail $ unpackConsList (snd $ eval ctx (head args))))
    where argsNum = length args

lispPrepend :: Context -> [Expr] -> (Context, Expr)
lispPrepend ctx args
    | argsNum /= 2 = error $ "':' expected 2 arguemnts, but got " ++ show argsNum
    | otherwise = (ctx, LispConsList $ unpackConsList (head eval_args) ++ unpackConsList (eval_args !! 1))
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

lispListLength :: Context -> [Expr] -> (Context, Expr)
lispListLength ctx args 
    | argsNum /= 1 = error $ "'length' expected 1 arguemnt, but got " ++ show argsNum
    | otherwise = (ctx, LispInteger $ genericLength $ unpackConsList (head eval_args))
    where argsNum = length args
          eval_args = map (snd . eval ctx) args

builtInMap :: Map String (Context -> [Expr] -> (Context, Expr))
builtInMap = Map.fromList [("+", lispNumAdd), 
                           ("-", lispNumSub), 
                           ("*", lispNumMul), 
                           ("/", lispNumDiv), 
                           ("%", lispNumMod),
                           ("concat", lispStrConcat),
                           ("=", lispNumEq), 
                           ("/=", lispNumNEq),
                           ("booleq", lispBoolEq), 
                           ("boolNeq", lispBoolNEq),
                           ("streq", lispStrEq), 
                           ("strNeq", lispStrNEq),
                           ("and", lispAnd), 
                           ("or", lispOr),
                           ("if", lispIf), 
                           ("define", lispDefine), 
                           ("lambda", lispLambda),
                           ("defun", lispDefun),
                           ("head", lispHead),
                           ("tail", lispTail),
                           ("++", lispPrepend),
                           ("length", lispListLength)]

eval :: Context -> Expr -> (Context, Expr)
eval ctx val@(LispInteger _) = (ctx, val)
eval ctx val@(LispBoolean _) = (ctx, val)
eval ctx val@(LispString _) = (ctx, val)
eval ctx val@(LispFunction _) = (ctx, val)
eval ctx val@(LispSymbol symbol) = (ctx, case (Map.lookup symbol ctx) of 
                                            Just v -> v
                                            Nothing -> val)

eval ctx (LispConsList lst) = (ctx, LispConsList $ map (snd . eval ctx) lst)
eval ctx (LispRangeList begin end) = (ctx, LispConsList (map LispInteger [(unpackInteger $ snd $ eval ctx begin) .. (unpackInteger $ snd $ eval ctx end)]))
eval ctx (LispRangeList2 begin begin2 end) = (ctx, LispConsList (map LispInteger [(unpackInteger $ snd $ eval ctx begin), (unpackInteger $ snd $ eval ctx begin2) .. (unpackInteger $ snd $ eval ctx end)]))

eval ctx (LispList (LispSymbol func : args)) = case Map.lookup func builtInMap of 
                                                    Nothing -> (unpackFunc $ snd $ eval ctx (LispSymbol func)) ctx (map (snd . eval ctx) args)
                                                    Just lispfunc -> lispfunc ctx args
eval ctx (LispList (LispList func : args)) = (unpackFunc $ snd $ eval ctx (LispList func)) ctx args                        

eval _ _ = error "Undefined eval function"