module Eval where

import Data.Map (Map)         
import qualified Data.Map as Map
import Data.List
import Data.Fixed
import Data.Maybe

import Expr
import Unpack
import ExprTypeCheck

allTheSame :: (Eq a) => [a] -> Bool
allTheSame xs = all (== head xs) (tail xs)

lispNumAdd :: Context -> [Expr] -> (Context, Expr)
lispNumAdd ctx args 
    | isFloatExpr $ head evalArgs = (ctx, LispFloat $ sum $ map unpackFloat evalArgs)
    | isIntegerExpr $ head evalArgs = (ctx, LispInteger $ sum $ map unpackInteger evalArgs)
    | otherwise = (ctx, LispError "function '+' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispNumSub :: Context -> [Expr] -> (Context, Expr)
lispNumSub ctx args
    | argsNum /= 2 = (ctx, LispError $ "'-' expected 2 arguments, but got " ++ show argsNum)
    | isFloatExpr $ head evalArgs = (ctx, LispFloat $ unpackFloat (head evalArgs) - unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispInteger $ unpackInteger (head evalArgs) - unpackInteger (evalArgs !! 1))
    | otherwise = (ctx, LispError "function '-' is not defined for the type")
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispNumMul :: Context -> [Expr] -> (Context, Expr)
lispNumMul ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispFloat $ product $ map unpackFloat evalArgs)
    | isIntegerExpr $ head evalArgs = (ctx, LispInteger $ product $ map unpackInteger evalArgs)
    | otherwise = (ctx, LispError "function '*' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispNumDiv :: Context -> [Expr] -> (Context, Expr)
lispNumDiv ctx args 
    | argsNum /= 2 = (ctx, LispError $ "'/' expected 2 arguments, but got " ++ show argsNum)
    | isFloatExpr $ head evalArgs = (ctx, LispFloat $ unpackFloat (head evalArgs) / unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispInteger $ unpackInteger (head evalArgs) `div` unpackInteger (evalArgs !! 1))
    | otherwise = (ctx, LispError "function '-' is not defined for the type")
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispNumMod :: Context -> [Expr] -> (Context, Expr)
lispNumMod ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispFloat $ foldl1 mod' (map unpackFloat evalArgs))
    | isIntegerExpr $ head evalArgs = (ctx, LispInteger $ foldl1 mod (map unpackInteger evalArgs))
    | otherwise = (ctx, LispError "function '/' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispEq :: Context -> [Expr] -> (Context, Expr)
lispEq ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ allTheSame (map unpackFloat evalArgs))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ allTheSame (map unpackInteger evalArgs))
    | isBooleanExpr $ head evalArgs = (ctx, LispBoolean $ allTheSame (map unpackBool evalArgs))
    | otherwise = (ctx, LispError "function '=' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispNEq :: Context -> [Expr] -> (Context, Expr)
lispNEq ctx args 
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ not $ allTheSame (map unpackFloat evalArgs))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ not $ allTheSame (map unpackInteger evalArgs))
    | isBooleanExpr $ head evalArgs = (ctx, LispBoolean $ not $ allTheSame (map unpackBool evalArgs))
    | otherwise = (ctx, LispError "function '/=' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispGreater :: Context -> [Expr] -> (Context, Expr)
lispGreater ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ unpackFloat (head evalArgs) > unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ unpackInteger (head evalArgs) > unpackInteger (evalArgs !! 1))    
    | otherwise = (ctx, LispError "function '>' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispGreaterEq :: Context -> [Expr] -> (Context, Expr)
lispGreaterEq ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ unpackFloat (head evalArgs) >= unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ unpackInteger (head evalArgs) >= unpackInteger (evalArgs !! 1))    
    | otherwise = (ctx, LispError "function '>=' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispLess :: Context -> [Expr] -> (Context, Expr)
lispLess ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ unpackFloat (head evalArgs) < unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ unpackInteger (head evalArgs) < unpackInteger (evalArgs !! 1))    
    | otherwise = (ctx, LispError "function '<' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispLessEq :: Context -> [Expr] -> (Context, Expr)
lispLessEq ctx args
    | isFloatExpr $ head evalArgs = (ctx, LispBoolean $ unpackFloat (head evalArgs) <= unpackFloat (evalArgs !! 1))
    | isIntegerExpr $ head evalArgs = (ctx, LispBoolean $ unpackInteger (head evalArgs) <= unpackInteger (evalArgs !! 1))    
    | otherwise = (ctx, LispError "function '>' is not defined for the type")
    where evalArgs = map (snd . eval ctx) args

lispAnd :: Context -> [Expr] -> (Context, Expr)
lispAnd ctx args = (ctx, LispBoolean $ all unpackBool evalArgs)
    where evalArgs = map (snd . eval ctx) args

lispOr :: Context -> [Expr] -> (Context, Expr)
lispOr ctx args = (ctx, LispBoolean $ any unpackBool evalArgs)
    where evalArgs = map (snd . eval ctx) args

lispIf :: Context -> [Expr] -> (Context, Expr)
lispIf ctx args
    | argsNum /= 3 = (ctx, LispError $ "'if' expected 3 arguments, but got " ++ show argsNum)
    | otherwise =
        if unpackBool $ snd $ eval ctx (head evalArgs) then eval ctx (evalArgs !! 1) else eval ctx (evalArgs !! 2)
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispDefine :: Context -> [Expr] -> (Context, Expr)
lispDefine ctx args 
    | Map.notMember strHeadArg ctx =
        (Map.insert strHeadArg (evalArgs !! 1) ctx, evalArgs !! 1)
    | otherwise = (ctx, LispError $ "symbol name already bound \"" ++ strHeadArg ++ "\"")
    where strHeadArg = show $ head args
          evalArgs = map (snd . eval ctx) args

lispLambda :: Context -> [Expr] -> (Context, Expr)
lispLambda ctx args
    | argsNum /= 2 = (ctx, LispError $ "'lambda' expected 2 arguments, but got " ++ show argsNum)
    | otherwise = (ctx, LispFunction $ \ctx2 argList ->
        let merge_map = Map.fromList $ Map.toList ctx2 ++ zip (map unpackSymbol defArgList) argList ++ Map.toList ctx
        in if length defArgList == length argList 
           then (ctx2, snd $ eval merge_map (args !! 1)) 
           else (ctx, LispError $ "expected " ++ show (length defArgList) ++ " argument(s), but got " ++ show (length argList)))
    where argsNum = length args
          defArgList = unpackList (head args)

lispDefun :: Context -> [Expr] -> (Context, Expr)
lispDefun ctx args
    | argsNum /= 3 = (ctx, LispError $ "'defun' expected 3 arguments, but got " ++ show argsNum)
    | otherwise = lispDefine ctx [head args, snd $ lispLambda ctx (tail args)]
    where argsNum = length args

lispHead :: Context -> [Expr] -> (Context, Expr)
lispHead ctx args
    | argsNum /= 1 = (ctx, LispError $ "'head' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, snd $ eval ctx (head $ unpackDataList (snd $ eval ctx (head args))))
    where argsNum = length args

lispTail :: Context -> [Expr] -> (Context, Expr)
lispTail ctx args
    | argsNum /= 1 = (ctx, LispError $ "'tail' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, snd $ eval ctx (LispDataList $ tail $ unpackDataList (snd $ eval ctx (head args))))
    where argsNum = length args

lispPrepend :: Context -> [Expr] -> (Context, Expr)
lispPrepend ctx args
    | argsNum /= 2 = (ctx, LispError $ "':' expected 2 arguemnts, but got " ++ show argsNum)
    | otherwise = (ctx, LispDataList $ unpackDataList (head evalArgs) ++ unpackDataList (evalArgs !! 1))
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispListLength :: Context -> [Expr] -> (Context, Expr)
lispListLength ctx args 
    | argsNum /= 1 = (ctx, LispError $ "'length' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, LispInteger $ genericLength $ unpackDataList (head evalArgs))
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispQuote :: Context -> [Expr] -> (Context, Expr)
lispQuote ctx args
    | argsNum /= 1 = (ctx, LispError $ "'quote' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = (ctx, head args)
    where argsNum = length args

lispEval :: Context -> [Expr] -> (Context, Expr)
lispEval ctx args
    | argsNum /= 1 = (ctx, LispError $ "'eval' expected 1 arguemnt, but got " ++ show argsNum)
    | otherwise = eval ctx (head evalArgs)
    where argsNum = length args
          evalArgs = map (snd . eval ctx) args

lispLet :: Context -> Context -> [Expr] -> (Context, Expr)
lispLet initCtx ctx [arg] = (initCtx, snd $ eval ctx arg)
lispLet initCtx ctx (arg:args) = lispLet initCtx (Map.insert sym val ctx) args
    where sym = unpackSymbol $ head $ unpackList arg
          val = snd $ eval ctx (unpackList arg !! 1)

lispApply :: Context -> [Expr] -> (Context, Expr)
lispApply ctx args
    | argsNum /= 2 = (ctx, LispError $ "'apply' expected 2 arguemnt, but got " ++ show argsNum)
    | otherwise = eval ctx (LispList (head args : unpackDataList (snd $ eval ctx (args !! 1))))
        where argsNum = length args

specialFuncMap :: Context -> Map String (Context -> [Expr] -> (Context, Expr))
specialFuncMap ctx = Map.fromList [("if", lispIf), 
                                   ("define", lispDefine), 
                                   ("lambda", lispLambda),
                                   ("defun", lispDefun),
                                   ("let", lispLet ctx)]

builtinMap :: Context
builtinMap = Map.fromList [("+", LispFunction lispNumAdd), 
                           ("-", LispFunction lispNumSub), 
                           ("*", LispFunction lispNumMul), 
                           ("/", LispFunction lispNumDiv), 
                           ("%", LispFunction lispNumMod),
                           ("=", LispFunction lispEq), 
                           ("/=", LispFunction lispNEq),
                           (">", LispFunction lispGreater),
                           (">=", LispFunction lispGreaterEq),
                           ("<", LispFunction lispLess),
                           ("<=", LispFunction lispLessEq),
                           ("and", LispFunction lispAnd), 
                           ("or", LispFunction lispOr),
                           ("if", LispFunction lispIf), 
                           ("head", LispFunction lispHead),
                           ("tail", LispFunction lispTail),
                           ("++", LispFunction lispPrepend),
                           ("length", LispFunction lispListLength),
                           ("quote", LispFunction lispQuote),
                           ("eval", LispFunction lispEval),
                           ("apply", LispFunction lispApply)]

eval :: Context -> Expr -> (Context, Expr)
eval ctx val@(LispFloat _) = (ctx, val)
eval ctx val@(LispInteger _) = (ctx, val)
eval ctx val@(LispBoolean _) = (ctx, val)
eval ctx val@(LispChar _) = (ctx, val)
eval ctx val@(LispFunction _) = (ctx, val)
eval ctx val@(LispSymbol symbol) = (ctx, fromMaybe (LispError $ "unbound symbol '" ++ show val ++ "'") (Map.lookup symbol ctx))

eval ctx (LispDataList lst) = (ctx, LispDataList $ map (snd . eval ctx) lst)
eval ctx (LispRangeList begin end) 
    | isFloatExpr eval_begin = (ctx, LispDataList (map LispFloat [(unpackFloat eval_begin) .. (unpackFloat eval_end)]))    
    | isIntegerExpr eval_begin = (ctx, LispDataList (map LispInteger [(unpackInteger eval_begin) .. (unpackInteger eval_end)]))
    | isCharExpr eval_begin = (ctx, LispDataList (map LispChar [(unpackChar eval_begin) .. (unpackChar eval_end)]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin
          eval_end = snd $ eval ctx end

eval ctx (LispRangeList2 begin begin2 end) 
    | isFloatExpr eval_begin = (ctx, LispDataList (map LispFloat [(unpackFloat eval_begin), (unpackFloat eval_begin2) .. (unpackFloat eval_end)]))    
    | isIntegerExpr eval_begin = (ctx, LispDataList (map LispInteger [(unpackInteger eval_begin), (unpackInteger eval_begin2) .. (unpackInteger eval_end)]))
    | isCharExpr eval_begin = (ctx, LispDataList (map LispChar [(unpackChar eval_begin), (unpackChar eval_begin2) .. (unpackChar eval_end)]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin
          eval_begin2 = snd $ eval ctx begin2
          eval_end = snd $ eval ctx end

eval ctx (LispInfRangeList begin) 
    | isFloatExpr eval_begin = (ctx, LispDataList (map LispFloat [(unpackFloat eval_begin) ..]))    
    | isIntegerExpr eval_begin = (ctx, LispDataList (map LispInteger [(unpackInteger eval_begin) ..]))
    | isCharExpr eval_begin = (ctx, LispDataList (map LispChar [(unpackChar eval_begin) ..]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin

eval ctx (LispInfRangeList2 begin begin2) 
    | isFloatExpr eval_begin = (ctx, LispDataList (map LispFloat [(unpackFloat eval_begin), (unpackFloat eval_begin2) ..]))    
    | isIntegerExpr eval_begin = (ctx, LispDataList (map LispInteger [(unpackInteger eval_begin), (unpackInteger eval_begin2) ..]))
    | isCharExpr eval_begin = (ctx, LispDataList (map LispChar [(unpackChar eval_begin), (unpackChar eval_begin2) ..]))
    | otherwise = (ctx, LispError "range list is not defined for the type")
    where eval_begin = snd $ eval ctx begin
          eval_begin2 = snd $ eval ctx begin2

eval ctx (LispList (LispSymbol func : args)) = case Map.lookup func (specialFuncMap ctx) of 
                                                    Nothing -> (unpackFunc $ snd $ eval ctx (LispSymbol func)) ctx (map (snd . eval ctx) args)
                                                    Just lispfunc -> lispfunc ctx args
eval ctx (LispList (LispFunction func : args)) = func ctx args                                           
eval ctx (LispList (LispList func : args)) = eval ctx (LispList (snd (eval ctx (LispList func)) : args))

eval ctx (LispDo [expr]) = eval ctx expr
eval ctx (LispDo (expr : exprs)) = let val = eval ctx expr
                                   in if isErrorExpr (snd val) then (ctx, snd val) else eval (fst val) (LispDo exprs) 
              
eval ctx err@(LispError _) = (ctx, err)
eval ctx _ = (ctx, LispError "undefined eval function")