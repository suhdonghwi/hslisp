module Unpack where

import Expr

unpackInteger :: Expr -> Integer
unpackInteger (LispInteger val) = val
unpackInteger _ = error "Expected Integer type"

unpackBool :: Expr -> Bool
unpackBool (LispBoolean val) = val
unpackBool _ = error "Expected Boolean type"

unpackString :: Expr -> String
unpackString (LispString val) = val
unpackString _ = error "Expected String type"

unpackFunc :: Expr -> Context -> [Expr] -> (Context, Expr)
unpackFunc (LispFunction val) = val
unpackFunc _ = error "Expected Function type"

unpackSymbol :: Expr -> String
unpackSymbol (LispSymbol val) = val
unpackSymbol _ = error "Expected Symbol type"

unpackConsList :: Expr -> [Expr]
unpackConsList (LispConsList val) = val
unpackConsList _ = error "Expected ConsList type"

unpackList :: Expr -> [Expr]
unpackList (LispList val) = val
unpackList _ = error "Exepcted List type"