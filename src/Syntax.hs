module Syntax where

import           Data.List (intercalate)

type Name = String

data BinOp = Add | Sub | Mul | Eq | Lt

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Eq  = "=="
  show Lt  = "<"

data Expr = Var Name
  | LInt Int
  | LBool Bool
  | BinExpr BinOp Expr Expr
  | IfExpr Expr Expr Expr
  | LetExpr [(Name, Expr)] Expr
  | Lambda Name Expr
  | App Expr Expr
  -- letrecの束縛では1引数以上の関数のみ定義できる
  | LetRecExpr [(Name, Name, Expr)] Expr

instance Show Expr where
  show (Var x) = x
  show (LInt n) = show n
  show (LBool b) = show b
  show (BinExpr op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
  show (IfExpr e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
  show (LetExpr bs bod) =
    "let " ++ intercalate " and " [x ++ " = " ++ show e | (x,e) <- bs] ++ " in " ++ show bod
  show (Lambda x e) = "fun " ++ x ++ " -> " ++ show e
  show (App e1 e2@Var{}) = unwords [show e1, show e2]
  show (App e1 e2@LInt{}) = unwords [show e1, show e2]
  show (App e1 e2@LBool{}) = unwords [show e1, show e2]
  show (App e1 e2) = show e1 ++ " (" ++ show e2 ++ ")"
  show (LetRecExpr _ _) = "let rec ..."

-- statement
data Stmt = ExprStmt Expr
   | LetStmt [(Name,Expr)]
   | LetRecStmt [(Name,Name,Expr)]
   deriving Show
