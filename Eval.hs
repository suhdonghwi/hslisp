module Eval where

import Data.Data
import Data.Map (Map)         
import qualified Data.Map as Map
import Data.Maybe

import Expr

type Context = Map String Expr

unpackNum :: Expr -> Integer
unpackNum (LispInteger val) = val
unpackNum _ = error "Expected Integer type"

unpackBool :: Expr -> Bool
unpackBool (LispBoolean val) = val
unpackBool _ = error "Expected Boolean type"

unpackString :: Expr -> String
unpackString (LispString val) = val
unpackString _ = error "Expected String type"

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

lispAdd :: Context -> [Expr] -> (Context, Expr)
lispAdd ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispInteger $ sum $ map unpackNum eval_args)
        "LispString" -> (ctx, LispString $ concatMap unpackString eval_args)
        _ -> error "function '+' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispSub :: Context -> [Expr] -> (Context, Expr)
lispSub ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispInteger $ foldl1 (-) (map unpackNum eval_args))
        _ -> error "function '-' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispMul :: Context -> [Expr] -> (Context, Expr)
lispMul ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispInteger $ product $ map unpackNum eval_args)
        _ -> error "function '*' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispDiv :: Context -> [Expr] -> (Context, Expr)
lispDiv ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispInteger $ foldl1 div (map unpackNum eval_args))
        _ -> error "function '/' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispMod :: Context -> [Expr] -> (Context, Expr)
lispMod ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispInteger $ foldl1 mod (map unpackNum eval_args))
        _ -> error "function '%' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispEq :: Context -> [Expr] -> (Context, Expr)
lispEq ctx args =
    case constr of
        "LispInteger" -> (ctx, LispBoolean $ allTheSame (map unpackNum eval_args))
        "LispBoolean" -> (ctx, LispBoolean $ allTheSame (map unpackBool eval_args))
        "LispString" -> (ctx, LispBoolean $ allTheSame (map unpackString eval_args))
        _ -> error "function '=' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispNEq :: Context -> [Expr] -> (Context, Expr)
lispNEq ctx args = 
    case constr of
        "LispInteger" -> (ctx, LispBoolean $ not $ allTheSame (map unpackNum eval_args))
        "LispBoolean" -> (ctx, LispBoolean $ not $ allTheSame (map unpackBool eval_args))
        "LispString" -> (ctx, LispBoolean $ not $ allTheSame (map unpackString eval_args))    
        _ -> error "function '/=' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispAnd :: Context -> [Expr] -> (Context, Expr)
lispAnd ctx args =
    case constr of
        "LispBoolean" -> (ctx, LispBoolean $ all unpackBool eval_args)
        _ -> error "function 'and' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispOr :: Context -> [Expr] -> (Context, Expr)
lispOr ctx args = 
    case constr of
        "LispBoolean" -> (ctx, LispBoolean $ any unpackBool eval_args)
        _ -> error "function 'or' is undefined for the type"
    where constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispIf :: Context -> [Expr] -> (Context, Expr)
lispIf ctx args
    | argsNum /= 3 = error $ "'if' expected 3 arguments, but got " ++ show argsNum
    | otherwise =
        case constr of 
            "LispBoolean" -> if unpackBool $ snd $ eval ctx (head eval_args) then eval ctx (eval_args !! 1) else eval ctx (eval_args !! 2)
            _ -> error "function 'if' is undefined for the type"
    where argsNum = length args
          constr = show $ toConstr (head eval_args)
          eval_args = map (snd . eval ctx) args

lispDefine :: Context -> [Expr] -> (Context, Expr)
lispDefine ctx args 
    | Map.notMember strHeadArg ctx =
        case constr of 
            "LispSymbol" -> (Map.insert strHeadArg (eval_args !! 1) ctx, (eval_args !! 1))
            _ -> error "expected symbol in first argument of define"
    | otherwise = error $ "symbol name already bound \"" ++ strHeadArg ++ "\""
    where constr = show $ toConstr (head args)
          strHeadArg = (show $ head args)
          eval_args = map (snd . eval ctx) args

funcMap :: Map String (Context -> [Expr] -> (Context, Expr))
funcMap = Map.fromList [("+", lispAdd), ("-", lispSub), ("*", lispMul), ("/", lispDiv), ("%", lispMod),
                            ("=", lispEq), ("/=", lispNEq), ("and", lispAnd), ("or", lispOr),
                            ("if", lispIf), ("define", lispDefine)]

eval :: Context -> Expr -> (Context, Expr)
eval ctx val@(LispInteger _) = (ctx, val)
eval ctx val@(LispBoolean _) = (ctx, val)
eval ctx val@(LispString _) = (ctx, val)
eval ctx val@(LispSymbol symbol) = (ctx, case (Map.lookup symbol ctx) of 
                                            Just v -> v
                                            Nothing -> val)
                                            
eval ctx (LispList (LispSymbol func : args)) = case Map.lookup func funcMap of 
                                                    Nothing -> error $ "Undefined function \"" ++ func ++ "\""
                                                    Just lispfunc -> lispfunc ctx args -- (map (snd . eval ctx) args)
eval _ _ = error "Undefined eval function"