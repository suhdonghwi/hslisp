module Parse where

import Text.ParserCombinators.Parsec

import Expr

parseExpr :: Parser Expr
parseExpr = try parseInteger <|> try parseBoolean <|> parseString <|> parseSymbol<|> parseList

parseInteger :: Parser Expr
parseInteger = do sign <- option "" (string "+" <|> string "-")
                  i <- many1 digit
                  return $ LispInteger (read ((if sign == "-" then sign else "") ++ i))

parseBoolean :: Parser Expr
parseBoolean = do p <- string "true" <|> string "false"
                  return $ LispBoolean (p == "true")

parseString :: Parser Expr
parseString = do _ <- char '"'
                 str <- many (noneOf "\"")
                 _ <- char '"'
                 return $ LispString str

parseSymbol :: Parser Expr
parseSymbol = do first <- firstChar
                 trailing <- many (firstChar <|> digit)
                 return $ LispSymbol (first:trailing)
                 where firstChar = oneOf "+-*/%=" <|> letter

parseList :: Parser Expr
parseList = do _ <- char '('
               lst <- sepBy parseExpr spaces
               _ <- char ')'
               return $ LispList lst