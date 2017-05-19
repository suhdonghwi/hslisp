module ExprTypeCheck where

import Expr

isFloatExpr :: Expr -> Bool
isFloatExpr (LispFloat _) = True
isFloatExpr _ = False

isIntegerExpr :: Expr -> Bool
isIntegerExpr (LispInteger _) = True
isIntegerExpr _ = False

isBooleanExpr :: Expr -> Bool
isBooleanExpr (LispBoolean _) = True
isBooleanExpr _ = False

isCharExpr :: Expr -> Bool
isCharExpr (LispChar _) = True
isCharExpr _ = False

isFunctionExpr :: Expr -> Bool
isFunctionExpr (LispFunction _) = True
isFunctionExpr _ = False

isSymbolExpr :: Expr -> Bool
isSymbolExpr (LispSymbol _) = True
isSymbolExpr _ = False

isConsListExpr :: Expr -> Bool
isConsListExpr (LispDataList _) = True
isConsListExpr _ = False

isListExpr :: Expr -> Bool
isListExpr (LispList _) = True
isListExpr _ = False

isErrorExpr :: Expr -> Bool
isErrorExpr (LispError _) = True
isErrorExpr _ = False