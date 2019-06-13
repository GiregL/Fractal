module Evaluator where

import Parser

import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromJust)
import Data.Type.Equality
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map as Map

-- | Environnement d'une fonction, contient les noms et leurs valeurs
data Env = Env (Map.Map String Expr)

-- | Création d'un environnement vide
emptyEnv :: Env
emptyEnv = Env Map.empty

-- | Récupération du contenu de l'environnement
toMap :: Env -> Map.Map String Expr
toMap (Env e) = e

-- | Fonctions builtin du langage
{-globalEnv :: Env
globalEnv = Env $ Map.fromList 
    [ ("+", numericFold (+) 0)
    , ("*", numericFold (*) 1)
    , ("-", minus)
    , ("/", divide)
    ]
-}

extractInteger :: Expr -> Maybe Integer
extractInteger (Constant (IntegerValue val)) = Just val
extractInteger _ = Nothing

extractDouble :: Expr -> Maybe Double
extractDouble (Constant (DoubleValue val)) = Just val
extractDouble _ = Nothing

--numericFold :: Expr -> Maybe Double
--numericFold (FunctionCall _ exprs) =

typeCheck :: [Expr] -> Expr -> Bool
typeCheck vals comparator =
    case (head vals) == comparator of
        True -> length vals == 1 || typeCheck (tail vals) comparator
        False -> False