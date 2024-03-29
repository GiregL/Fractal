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
import Data.Type.Equality

{-
    Définition de l'AST
-}

-- | Type représentant un programme en Fractal
type Program = [Expr]

-- | Type représentant une valeur constante du langage
data Value
    = IntegerValue Integer
    | DoubleValue Double
    | CharValue Char
    | StringValue String
    | BoolValue Bool
    | SymbolValue String
    | Null
    deriving (Show)

instance Eq Value where
    (==) _ Null = True
    (==) Null _ = True
    (==) (IntegerValue v) (IntegerValue v2) = v == v2
    (==) (IntegerValue _) _ = False
    (==) (DoubleValue v) (DoubleValue v2) = v == v2
    (==) (DoubleValue _) _ = False
    (==) (CharValue v) (CharValue v2) = v == v2
    (==) (CharValue _) _ = False
    (==) (StringValue v) (StringValue v2) = v == v2
    (==) (StringValue _) _ = False
    (==) (BoolValue v) (BoolValue v2) = v == v2
    (==) (BoolValue _) _ = False
    (==) (SymbolValue v) (SymbolValue v2) = v == v2
    (==) (SymbolValue _) _ = False

-- | Type représentant l'arbre syntaxique du langage
data Expr 
    = Constant Value
    | Exception String                  -- Cas spécifique, non implémenté dans la syntaxe
    | Quote [Expr]
    | FunctionCall Value [Expr]
    | IfExpr Expr [Expr] [Expr]             -- If Predicat Then Else
    | DefineExpr Value [Value] [Expr]      -- Define nom args body
    | Action Expr [Expr]                  -- Predicate Action
    | Cond [Expr]                       -- Série d'Action
    deriving (Show)

instance Eq Expr where
    (==) (Exception _) (Exception _) = True
    (==) (Exception _) _ = False
    (==) (Quote _) (Quote _) = True
    (==) (Quote _) _ = False
    (==) (Constant v) (Constant v2) = v == v2
    (==) (Constant _) _ = False
    (==) (FunctionCall _ _) (FunctionCall _ _) = True
    (==) (FunctionCall _ _) _ = True
    (==) (IfExpr _ _ _) (IfExpr _ _ _) = True
    (==) (IfExpr _ _ _) _ = False
    (==) (DefineExpr _ _ _) (DefineExpr _ _ _) = True
    (==) (DefineExpr _ _ _) _ = False
    (==) (Action _ _) (Action _ _) = True
    (==) (Action _ _) _ = False
    (==) (Cond _) (Cond _) = True
    (==) (Cond _) _ = False

{-
-- TODO: Finir le pretty printer
instance Show Expr where
    show = prettyPrint 0

prettyPrint :: Int -> Expr -> String
prettyPrint n (Constant v) = (replicate n '\t') ++ show v
prettyPrint n (Quote q) = concatMap (("\n" ++ replicate (n + 1) '\t') ++) (map show q)
prettyPrint _ _ = "Not yet implemented"
-}

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
    c <- noneOf "'"
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
    content <- many1 $ satisfy (/= '"')
    _ <- char '"'
    return $ StringValue content

-- | Parser d'un Symbol
symbolP :: Parser Value
symbolP = do
    f <- firstLetter
    r <- rest
    return $ SymbolValue (f:r)
    where firstLetter = letter <|> oneOf "_?!+-*/%"
          rest = many (letter <|> digit <|> oneOf "_?!+-*/%") 

-- | Parser d'une Value
value :: Parser Value
value = do
        whiteSpace
        res <- choice parsers
        whiteSpace
        return res
        where parsers = map try [stringP, charP, double, integer, boolean, symbolP]

-- | Parser d'un Quote
-- | Exemple : `'(#True #False #False)`
quote :: Parser Expr
quote = do
    _ <- string "'("
    content <- many1 expr
    _ <- char ')'
    return $ Quote content

-- | Parser d'un appel de fonction
functionCall :: Parser Expr
functionCall = do
    _ <- char '('
    id <- symbolP
    body <- many1 expr
    _ <- char ')'
    return $ FunctionCall id body

-- | Parser d'une condition If
ifExpr :: Parser Expr
ifExpr = do
    _ <- char '('
    whiteSpace
    _ <- string "If"
    whiteSpace
    _ <- char '['
    predicate <- expr
    _ <- char ']'
    whiteSpace
    _ <- char '{'
    thenClause <- many expr
    _ <- char '}'
    whiteSpace
    _ <- char '{'
    elseClause <- many expr
    _ <- char '}'
    whiteSpace
    _ <- char ')'
    whiteSpace
    return $ IfExpr predicate thenClause elseClause

-- | Parser d'un bloc define
defineExpr :: Parser Expr
defineExpr = do
    _ <- char '('
    whiteSpace
    _ <- string "Define"
    whiteSpace
    name <- symbolP
    whiteSpace
    _ <- char '['
    params <- many symbolP
    _ <- char ']'
    whiteSpace
    _ <- char '{'
    whiteSpace
    body <- many expr
    whiteSpace
    _ <- char '}'
    whiteSpace
    _ <- char ')'
    whiteSpace
    return $ DefineExpr name params body

-- | Parser d'un bloc Cond
condExpr :: Parser Expr
condExpr = do
    _ <- char '('
    whiteSpace
    _ <- string "Cond"
    whiteSpace
    content <- many1 actions
    whiteSpace
    _ <- char ')'
    whiteSpace
    return $ Cond content
    where actions = do
            whiteSpace
            _ <- char '('
            whiteSpace
            _ <- char '['
            whiteSpace
            predicate <- expr
            whiteSpace
            _ <- char ']'
            whiteSpace
            _ <- char '{'
            whiteSpace
            action <- many expr
            whiteSpace
            _ <- char '}'
            whiteSpace
            _ <- char ')'
            whiteSpace
            return $ Action predicate action

-- | Parser d'un commentaire uniligne
lineComment :: Parser ()
lineComment = do
    string "--"
    whiteSpace
    skipMany $ noneOf "\r\n\0"
    whiteSpace
    return ()

skipComments :: Parser ()
skipComments = skipMany lineComment

-- | Parser d'une expression
expr :: Parser Expr
expr = do
    whiteSpace
    skipMany lineComment
    res <- choice $ map try [ifExpr, defineExpr, quote, functionCall, (Constant <$> value)]
    whiteSpace
    return res

-- | Parser du programme
program :: Parser Program
program = many1 expr

-- | Résultat du Parser
result :: String -> Either ParseError Program
result = parse program ""

{-
        Utils
-}

-- | Passe d'une liste de Value à une liste de Constant
valueListToExprList :: [Value] -> [Expr]
valueListToExprList [] = []
valueListToExprList (x:[]) = [Constant x]
valueListToExprList (x:xs) = Constant x : valueListToExprList xs