module Expr where

import Data.Map (Map)

type Context = Map String Expr

data Expr = LispInteger Integer |
            LispBoolean Bool |
            LispString String |
            LispFunction (Context -> [Expr] -> (Context, Expr)) |
            LispSymbol String |
            LispConsList [Expr] |
            LispRangeList Expr Expr |
            LispRangeList2 Expr Expr Expr |
            LispList [Expr]

instance Show Expr where
    show (LispInteger val) = show val
    show (LispBoolean val) = if val then "true" else "false"
    show (LispString val) = "\"" ++ val ++ "\""
    show (LispFunction _) = "[function]"
    show (LispSymbol val) = val
    show (LispConsList val) = "[" ++ unwords (map show val) ++ "]"
    show (LispList val) = "(" ++ unwords (map show val) ++ ")"
    show _ = "[undefined]"