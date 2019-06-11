module Parser where

import Control.Monad
--import Control.Applicative ((<|>), many)

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Language

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Data.Char
import Data.Functor.Identity

{-
    Définition de l'AST
-}

-- | Type représentant une valeur constante du langage
data Value
    = IntegerValue Integer
    | DoubleValue Double
    | CharValue Char
    | StringValue String
    | BoolValue Bool
    | SymbolValue String
    deriving (Show)

-- | Type représentant un identifiant du langage
type Identifier = String

-- | Type représentant l'arbre syntaxique du langage
data Expr 
    = Constant Value
    | Quote [Expr]
    | List Identifier [Expr]

instance Show Expr where
    show = prettyPrint 0

printHelper :: Expr -> String
printHelper (Constant v) = show v
printHelper (Quote list) = concatMap show list
printHelper _ = "Not Implemented Yet"

prettyPrint :: Int -> Expr -> String
prettyPrint n (Constant v) = (replicate n '\t') ++ show v
prettyPrint n (Quote q) = concatMap (("\n" ++ replicate (n + 1) '\t') ++) (map show q)
prettyPrint _ _ = "Not yet implemented"


{-
    Parsers
-}

-- | Parser des whitespaces
whiteSpace :: Parser ()
whiteSpace = skipMany $ oneOf " \t\r\n"

-- | Parser d'une partie numérique
num :: Parser String
num = many1 digit

-- | Parser d'un Integer
integer :: Parser Value
integer = negInteger <|> posInteger
    where negInteger = do
            s <- char '-'
            rem <- num
            return $ IntegerValue (read (s:rem) :: Integer)
          posInteger = do
            content <- num
            return $ IntegerValue (read content :: Integer)

-- | Parser d'un Double
double :: Parser Value
double = negDouble <|> posDouble
    where negDouble = do
            s <- char '-'
            posPart <- num
            sep <- char '.'
            rem <- num
            return $ DoubleValue (read ([s] ++ posPart ++ [sep] ++ rem) :: Double)
          posDouble = do
            posPart <- num
            sep <- char '.'
            rem <- num
            return $ DoubleValue (read (posPart ++ [sep] ++ rem) :: Double)


-- | Parser d'un Char
charP :: Parser Value
charP = do
    _ <- char '\''
    c <- anyChar
    _ <- char '\''
    return $ CharValue c

-- | Parser d'un Boolean
boolean :: Parser Value
boolean = do
    _ <- char '#'
    false <|> true
    where true = do
            c <- string "True"
            return $ BoolValue True
          false = do
            c <- string "False"
            return $ BoolValue False

-- | Parser d'un String
stringP :: Parser Value
stringP = do
    _ <- char '"'
    content <- many anyChar
    _ <- char '"'
    return $ StringValue content

-- | Parser d'un Symbol
symbolP :: Parser Value
symbolP = do
    f <- firstLetter
    r <- rest
    return $ SymbolValue (f:r)
    where firstLetter = letter <|> oneOf "_?!"
          rest = many (letter <|> digit <|> oneOf "_?!") 

-- | Parser d'une Value
value :: Parser Value
value = do
        whiteSpace
        res <- choice parsers
        whiteSpace
        return res
        where parsers = map try [stringP, charP, double, integer, boolean, symbolP]

-- | Parser d'un identifiant du langage
identifier :: Parser String
identifier = do
    fc <- firstChar
    rest <- many nonFirstChar
    return (fc:rest)
    where
        firstChar = satisfy (\a -> isLetter a || a == '_')
        nonFirstChar = satisfy (\a -> isDigit a || isLetter a || a == '_' || a == '\'')



-- | Parser d'un Quote
-- | Exemple : `'(#True #False #False)`

quote :: Parser Expr
quote = do
    _ <- string "'("
    content <- many1 value
    _ <- char ')'
    return $ Quote (valueListToExprList content)


{-
        Utils
-}

-- | Passe d'une liste de Value à une liste de Constant
valueListToExprList :: [Value] -> [Expr]
valueListToExprList [] = []
valueListToExprList (x:[]) = [Constant x]
valueListToExprList (x:xs) = Constant x : valueListToExprList xs