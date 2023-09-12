module Typing.Type (Type(..), Scheme(..)) where

import qualified Data.Set as S (Set)
import           Syntax

data Type = TInt | TBool | TArrow Type Type | TVar Name
  deriving Eq

data Scheme = Forall (S.Set Name) Type

instance Show Type where
  show TInt           = "int"
  show TBool          = "bool"
  show (TArrow t1 t2)
    | TArrow _ _ <- t1 = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
    | otherwise = show t1 ++ " -> " ++ show t2
  show (TVar x)       = x

instance Show Scheme where
  show (Forall _ t) = show t
