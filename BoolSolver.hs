import Tautology
import Parsing

eqn :: Parser Prop
eqn = do i <- impl
         do symbol "=="
            <|> symbol "="
            <|> symbol "<=>"
            <|> symbol "<->"
         Equiv i <$> eqn
      <|> impl

impl :: Parser Prop
impl = do e <- exp' 
          do symbol "->"
             <|> symbol "=>"
          Imply e <$> impl
       <|> exp'

exp' :: Parser Prop
exp' = do t <- term
          do symbol "+"
             <|> symbol "||"
          Or t <$> exp'
       <|> term

term :: Parser Prop
term = do f <- factor
          do symbol "."
             <|> symbol "&&"
          And f <$> term
       <|> factor 

factor :: Parser Prop
factor = do do symbol "!"
               <|> symbol "~"
            Not <$> el
         <|> el

el :: Parser Prop
el = do symbol "("
        e <- eqn 
        symbol ")"
        return e
     <|> do symbol "False"
            return (Tautology.Const False)
     <|> do symbol "True"
            return (Tautology.Const True)
     <|> do space
            ch <- upper
            space
            return (Var ch)

solve :: String -> Maybe Bool
solve s = case parse eqn s of
            [(eq, "")] -> Just (isTaut eq)
            _ -> Nothing
