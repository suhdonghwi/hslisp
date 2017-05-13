{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import Data.Data

data Expr = LispInteger Integer |
            LispBoolean Bool |
            LispString String |
            LispSymbol String |
            LispList [Expr]
            deriving (Data,Typeable)

instance Show Expr where
    show (LispInteger val) = show val
    show (LispBoolean val) = if val then "True" else "False"
    show (LispString val) = "\"" ++ val ++ "\""
    show (LispSymbol val) = val
    show (LispList val) = "(" ++ unwords (map show val) ++ ")"