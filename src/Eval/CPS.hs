module Eval.CPS where

import           Control.Monad.Trans.Reader
import qualified Env                        as E
import           Error
import           Syntax

data Val = VInt Int
  | VBool Bool
  | VClosure Name Expr VEnv

instance Show Val where
  show (VInt n)      = show n
  show (VBool True)  = "true"
  show (VBool False) = "false"
  show VClosure{}    = "<closure>"

type VEnv = E.Env Val
type Eval a = ReaderT VEnv Check a

unpackInt :: Expr -> Eval Int
unpackInt ex = do
  v <- eval ex
  case v of
    VInt n -> return n
    _      -> liftThrow (Default ("unpackInt " ++ show ex))

unpackBool :: Expr -> Eval Bool
unpackBool ex = do
  v <- eval ex
  case v of
    VBool b -> return b
    _       -> liftThrow (Default ("unpackBool " ++ show ex))

unpackClosure :: Expr -> Eval (Name, Expr, VEnv)
unpackClosure ex = do
  v <- eval ex
  case v of
    VClosure x bod clo -> return (x,bod,clo)
    _                  -> liftThrow (Default ("unpackClosure " ++ show ex))

applyIntBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> Eval Val
applyIntBinOp op e1 e2 = do
  n1 <- unpackInt e1
  n2 <- unpackInt e2
  return (VInt (op n1 n2))

applyBoolBinOp :: (Int -> Int -> Bool) -> Expr -> Expr -> Eval Val
applyBoolBinOp op e1 e2 = do
  n1 <- unpackInt e1
  n2 <- unpackInt e2
  return (VBool (op n1 n2))

applyBinOp :: BinOp -> Expr -> Expr -> Eval Val
applyBinOp Add e1 e2 = applyIntBinOp (+) e1 e2
applyBinOp Sub e1 e2 = applyIntBinOp (-) e1 e2
applyBinOp Mul e1 e2 = applyIntBinOp (*) e1 e2
applyBinOp Lt e1 e2  = applyBoolBinOp (<) e1 e2

eval :: Expr -> Eval Val
eval (LInt n)  = return (VInt n)
eval (LBool b) = return (VBool b)
eval (Var x) = E.lookupVar x
eval (BinExpr op e1 e2) = applyBinOp op e1 e2
eval (IfExpr e1 e2 e3) = do
  b <- unpackBool e1
  if b then eval e2 else eval e3
eval (LetExpr bs bod) = do
  let (xs,es) = unzip bs
  vs <- mapM eval es
  local (E.bindVars (zip xs vs)) (eval bod)
eval (Lambda x bod) = VClosure x bod <$> ask
eval (App e1 e2) = do
  (x,bod,clo) <- unpackClosure e1
  arg <- eval e2
  let env' = E.bindVars [(x,arg)] clo
  local (const env') (eval bod)
eval (LetRecExpr bs bod) = do
  env <- ask
  let env' = E.bindVars [(f, VClosure x e env') | (f,x,e) <- bs] env
  local (const env') (eval bod)

runEval :: VEnv -> Expr -> Check Val
runEval env ex = runReaderT (eval ex) env

testEval :: Expr -> Check Val
testEval ex = runEval mempty ex
