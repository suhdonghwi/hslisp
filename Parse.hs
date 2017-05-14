module Parse where

import Text.ParserCombinators.Parsec

import Expr

parseExpr :: Parser Expr
parseExpr = try parseInteger <|> 
            try parseBoolean <|> 
            parseString <|> 
            parseSymbol <|> 
            try parseConsList <|> 
            try parseRangeList <|>
            try parseRangeList2 <|>
            parseList

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
                 where firstChar = oneOf "+-*/%=?" <|> letter

parseConsList :: Parser Expr
parseConsList = do _ <- char '['
                   lst <- sepBy parseExpr spaces
                   _ <- char ']'
                   return $ LispConsList lst

parseRangeList :: Parser Expr
parseRangeList = do char '['
                    begin <- parseExpr
                    spaces
                    char '~'
                    spaces
                    end <- parseExpr
                    _ <- char ']'
                    return $ LispRangeList begin end

parseRangeList2 :: Parser Expr
parseRangeList2 = do _ <- char '['
                     begin <- parseExpr
                     _ <- spaces
                     begin2 <- parseExpr
                     _ <- spaces
                     _ <- char '~'
                     _ <- spaces
                     end <- parseExpr
                     _ <- char ']'
                     return $ LispRangeList2 begin begin2 end

parseList :: Parser Expr
parseList = do _ <- char '('
               lst <- sepBy parseExpr spaces
               _ <- char ')'
               return $ LispList lst