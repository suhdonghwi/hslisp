module Eval where

import Data.Map (Map)         
import qualified Data.Map as Map
import Data.List

import Expr

unpackNum :: Expr -> Integer
unpackNum (LispInteger val) = val
unpackNum _ = error "Expected Integer type"

unpackBool :: Expr -> Bool
unpackBool (LispBoolean val) = val
unpackBool _ = error "Expected Boolean type"

unpackString :: Expr -> String
unpackString (LispString val) = val
unpackString _ = error "Expected String type"

unpackFunc :: Expr -> (Context -> [Expr] -> (Context, Expr))
unpackFunc (LispFunction val) = val
unpackFunc _ = error "Expected Function type"

unpackSymbol :: Expr -> String
unpackSymbol (LispSymbol val) = val
unpackSymbol _ = error "Expected Symbol type"

unpackList :: Expr -> [Expr]
unpackList (LispList val) = val
unpackList _ = error "Exepcted List type"

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

lispNumAdd :: Context -> [Expr] -> (Context, Expr)
lispNumAdd ctx args = (ctx, LispInteger $ sum $ map unpackNum eval_args)
    where eval_args = map (snd . eval ctx) args

lispStrConcat :: Context -> [Expr] -> (Context, Expr)
lispStrConcat ctx args = (ctx, LispString $ concatMap unpackString eval_args)
    where eval_args = map (snd . eval ctx) args

lispNumSub :: Context -> [Expr] -> (Context, Expr)
lispNumSub ctx args = (ctx, LispInteger $ foldl1 (-) (map unpackNum eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumMul :: Context -> [Expr] -> (Context, Expr)
lispNumMul ctx args = (ctx, LispInteger $ product $ map unpackNum eval_args)
    where eval_args = map (snd . eval ctx) args

lispNumDiv :: Context -> [Expr] -> (Context, Expr)
lispNumDiv ctx args = (ctx, LispInteger $ foldl1 div (map unpackNum eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumMod :: Context -> [Expr] -> (Context, Expr)
lispNumMod ctx args = (ctx, LispInteger $ foldl1 mod (map unpackNum eval_args))
    where eval_args = map (snd . eval ctx) args

lispNumEq :: Context -> [Expr] -> (Context, Expr)
lispNumEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackNum eval_args))
    where eval_args = map (snd . eval ctx) args

lispBoolEq :: Context -> [Expr] -> (Context, Expr)
lispBoolEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackBool eval_args))
    where eval_args = map (snd . eval ctx) args

lispStrEq :: Context -> [Expr] -> (Context, Expr)
lispStrEq ctx args = (ctx, LispBoolean $ allTheSame (map unpackString eval_args))
    where eval_args = map (snd . eval ctx) args 

lispNumNEq :: Context -> [Expr] -> (Context, Expr)
lispNumNEq ctx args = (ctx, LispBoolean $ not $ allTheSame (map unpackNum eval_args))
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
                           ("lambda", lispLambda)]

eval :: Context -> Expr -> (Context, Expr)
eval ctx val@(LispInteger _) = (ctx, val)
eval ctx val@(LispBoolean _) = (ctx, val)
eval ctx val@(LispString _) = (ctx, val)
eval ctx val@(LispFunction _) = (ctx, val)
eval ctx val@(LispSymbol symbol) = (ctx, case (Map.lookup symbol ctx) of 
                                            Just v -> v
                                            Nothing -> val)
                                            
eval ctx (LispList (LispSymbol func : args)) = case Map.lookup func builtInMap of 
                                                    Nothing -> (unpackFunc $ snd $ eval ctx (LispSymbol func)) ctx (map (snd . eval ctx) args)
                                                    Just lispfunc -> lispfunc ctx args
eval ctx (LispList (LispList func : args)) = (unpackFunc $ snd $ eval ctx (LispList func)) ctx args                        

eval _ _ = error "Undefined eval function"