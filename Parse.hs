module Parse where

import Text.ParserCombinators.Parsec

import Expr

parseExpr :: Parser Expr
parseExpr = try parseFloat <|>
            try parseInteger <|> 
            try parseBoolean <|> 
            try parseChar <|> 
            parseQuote <|>
            parseString <|>
            parseSymbol <|> 
            try parseConsList <|> 
            try parseRangeList <|>
            try parseRangeList2 <|>
            try parseInfRangeList <|>
            try parseInfRangeList2 <|>            
            parseList

parseFloat :: Parser Expr
parseFloat = do sign <- option "" (string "+" <|> string "-")
                integerPart <- many1 digit
                char '.'
                realPart <- many1 digit
                return $ LispFloat $ read $ (if sign == "-" then sign else "") ++ integerPart ++ "." ++ realPart

parseInteger :: Parser Expr
parseInteger = do sign <- option "" (string "+" <|> string "-")
                  i <- many1 digit
                  return $ LispInteger (read ((if sign == "-" then sign else "") ++ i))

parseBoolean :: Parser Expr
parseBoolean = do p <- string "true" <|> string "false"
                  return $ LispBoolean (p == "true")

parseChar :: Parser Expr
parseChar = do _ <- char '\''
               ch <- noneOf "'"
               _ <- char '\''
               return $ LispChar ch

parseQuote :: Parser Expr
parseQuote = do _ <- char '\''
                expr <- parseExpr
                return $ LispList [LispSymbol "quote", expr]

escape :: Parser String
escape = do
    d <- char '\\'
    c <- oneOf "\\\"0nrvtbf" -- all the characters which can be escaped
    return [d, c]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escape

getEscape :: Char -> Char
getEscape ch = case ch of 
                '\"' -> '\"'
                '0' -> '\0'
                'n' -> '\n'
                'r' -> '\r'
                'v' -> '\v'
                't' -> '\t'
                'b' -> '\b'
                'f' -> '\f'

transEscape :: String -> Char
transEscape str = if head str == '\\' then getEscape (str !! 1) else head str

parseString :: Parser Expr
parseString = do _ <- char '"'
                 str <- many character
                 _ <- char '"'
                 return $ LispConsList $ map (LispChar . transEscape) str

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

parseInfRangeList :: Parser Expr
parseInfRangeList = do char '['
                       begin <- parseExpr
                       spaces
                       char '~'
                       spaces
                       _ <- char ']'
                       return $ LispInfRangeList begin

parseInfRangeList2 :: Parser Expr
parseInfRangeList2 = do _ <- char '['
                        begin <- parseExpr
                        _ <- spaces
                        begin2 <- parseExpr
                        _ <- spaces
                        _ <- char '~'
                        _ <- spaces
                        _ <- char ']'
                        return $ LispInfRangeList2 begin begin2

parseList :: Parser Expr
parseList = do _ <- char '('
               lst <- sepEndBy parseExpr spaces
               _ <- char ')'
               return $ LispList lst

parseExprs :: Parser Expr
parseExprs = do exprs <- sepEndBy parseExpr spaces
                return $ LispDo exprs