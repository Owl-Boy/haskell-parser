module Parsers (module Parsers, module Parsing) where

import Parsing

{- 
  expr ::= term + expr | term
  term ::= factor * term | factor
  factor ::= nat * factor | nat
-}

expr :: Parser Int
expr = do t <- term
          symbol "+"
          e <- expr
          return (t + e)
       <|>do t <- term
             symbol "-"
             e <- expr
             return (t - e)
       <|> term

term :: Parser Int
term = do f <- factor
          symbol "*"
          t <- term
          return (f * t)
       <|> do f <- factor    
              symbol "/"
              t <- term
              return (f `div` t)
       <|> factor

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> expo

expo :: Parser Int
expo = do n <- natural
          symbol "^"
          m <- natural
          return (n ^ m)
       <|> natural

-- eval :: String -> Int
-- eval inp = case parse expr inp of
--              [(n, [])] -> n
--              [(_, out)] -> error ("unused import" ++ out)
--              _ -> error "wrong input"

comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             return ()
