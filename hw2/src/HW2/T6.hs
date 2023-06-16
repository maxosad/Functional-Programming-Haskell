{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HW2.T6 
  ( ParseError(..)
  , Parser(..)
  , runP
  , pChar
  , parseError 
  , parseExpr 
  , (<|>)
  ) where

import HW2.T1 hiding(Pair(..))
import HW2.T2
import HW2.T4 (Expr(..), Prim(..))
import HW2.T5
import Numeric.Natural (Natural)
import Control.Applicative 
import Control.Monad

data ParseError = ErrorAtPos Natural deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP (P es) str = per ((runES es) (0,str))

per :: Except ParseError (Annotated (Natural, String) a) -> Except ParseError a
per (Error e) = (Error e)
per (Success (a:#e)) = (Success a)


pChar :: Parser Char
pChar = P $ ES (\(pos, s) -> case s of
  []     -> Error (ErrorAtPos pos)
  (c:cs) -> Success (c :# (pos + 1, cs)))
{-
  когда строка пуста, общее состояние становится Error
  когда символ если не пуста, то 1ый символ откусывают от строки, говорят что он обработан
    и остается обработать символы после него. 
  Поддерживается позиция обрабатываемог символа, поэтому пред позиция увеличивается на 1.
  pChar берет 1ый символ в необработанной строке и изменяет позицию и необработанную строку
  , если строка пуста то состаяние становится Error
  
-}  
parseError :: Parser a  
parseError = P $ ES (\(pos, s) -> Error (ErrorAtPos pos))

parserSuccess :: ExceptState ParseError (Natural, String) Double
parserSuccess = ES {runES = \s -> wrapExcept (1.0:#s)}
     
  
instance Alternative Parser where
  empty = parseError
  (P e1) <|> (P e2) = P $ ES (\(pos,s) -> 
    let s1 = runES e1 (pos,s) in
      case s1 of
        (Error e) -> runES e2 (pos,s)
        _ -> s1
    ) 


instance MonadPlus Parser

pEof :: Parser ()
pEof = P $ ES (\(pos, s) -> case s of
  [] -> Success (() :# (pos + 1, ""))
  _  -> (Error (ErrorAtPos pos)))


parseExpr :: String -> Except ParseError Expr
parseExpr = runP mainexpr

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = pChar >>= (\c -> if f c 
  then pure c
  else empty
  )

 
read' :: String -> Double -- "123" -> 123.0
read' [] = 0.0
read' l = foldl f 0.0 l
 
f :: Double -> Char -> Double
f acc c = let a = acc * 10 in
  case c of
    '0' -> a 
    '1' -> a + 1.0
    '2' -> a + 2.0
    '3' -> a + 3.0
    '4' -> a + 4.0
    '5' -> a + 5.0
    '6' -> a + 6.0
    '7' -> a + 7.0
    '8' -> a + 8.0
    '9' -> a + 9.0

read'' :: String -> Double
read'' [] = 0.0
read'' l = foldr ff 0.0 l
 
ff :: Char -> Double -> Double -- "123" -> 0.123
ff c acc = let a = acc / 10 in
  case c of
    '0' -> a 
    '1' -> a + 0.1
    '2' -> a + 0.2
    '3' -> a + 0.3
    '4' -> a + 0.4
    '5' -> a + 0.5
    '6' -> a + 0.6
    '7' -> a + 0.7
    '8' -> a + 0.8
    '9' -> a + 0.9

r0 = read'' "1234"
r1 = read'' "0.1234"
r2 = read'' " dsf0.1234"

char :: Char -> Parser Char
char c = satisfy (== c)

space :: Parser String -- пробелов может и не быть 
space = many (char ' ')

isDigit' :: Char -> Bool
isDigit' '0' = True
isDigit' '1' = True
isDigit' '2' = True
isDigit' '3' = True
isDigit' '4' = True
isDigit' '5' = True
isDigit' '6' = True
isDigit' '7' = True
isDigit' '8' = True
isDigit' '9' = True
isDigit' _   = False

dot :: Parser Char
dot = char '.'

d0 = runP dot ".sd"
d1 = runP dot " .sd"
d2 = runP dot "1.sd"
d3 = runP dot " 1.sd"
d4 = runP dot ".. 1.sd"


d2e' :: String -> Expr 
d2e' str = (Val (read' str))

d2e'' :: String -> Expr 
d2e'' str = (Val (read'' str))

pde :: Parser Expr 
pde = do
  space
  cl <- fmap read' (some (satisfy isDigit'))
  c  <- optional dot
  dr <- optional (fmap read'' (some (satisfy isDigit')))
  space
  case (c,dr) of 
    (Nothing, _) ->  pure (Val cl) 
    ((Just '.'), Nothing) -> empty
    ((Just '.'), (Just dd)) -> pure (Val (cl + dd))


pde0 = runP pde "123.321" 
pde1 = runP pde "  123.321 123.321 " 
pde2 = runP pde "  123  .321  " 
pde3 = runP pde "  123.3s1  " 
pde4 = runP pde "  123.321  as" 
pde5 = runP pde "3.14"
pde6 = runP pde "wert"
pde7 = runP pde "12.wqe"


nextt :: Char -> Parser Char
nextt c = space *> char c

addsub :: Parser (Expr -> Expr -> Expr)
addsub = nextt '+' *> pure (+) <|> nextt '-' *> pure (-)

muldiv :: Parser (Expr -> Expr -> Expr)
muldiv = nextt '*' *> pure (*) <|> nextt '/' *> pure (/)


lasoc :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr
lasoc pe op = do 
  e1 <- pe
  con e1
  where 
    con e1 = (do o <- op; e2 <- pe; con (o e1 e2)) <|> return e1

mainexpr :: Parser Expr  
mainexpr = expr <* pEof

expr :: Parser Expr
expr = podv `lasoc` muldiv `lasoc` addsub
 
podv :: Parser Expr
podv = nextt '(' *> expr <* nextt ')' <|> pde

e0 = runP mainexpr "3.14 + 1.618 * 2"
e1 = runP mainexpr "2 * (1 + 3)"
e2 = runP mainexpr "24 + 3Hello"
e3 = runP mainexpr "3Hello"
e4 = runP mainexpr "3 ** 4"
e5 = runP mainexpr "3 & 5"
e6 = runP mainexpr ""
e7 = runP mainexpr "1 - 2 - 3"

tes = do 
  putStr (show e0 ++ "\n")
  putStr (show e1 ++ "\n")
  putStr (show e2 ++ "\n")
  putStr (show e3 ++ "\n")
  putStr (show e4 ++ "\n")
  putStr (show e5 ++ "\n")
  putStr (show e6 ++ "\n")
  putStr (show e7 ++ "\n")