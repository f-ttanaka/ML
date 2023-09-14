module Eval.ZAM.Compile where

import           Control.Monad.Trans.Reader
import           Data.List                  (elemIndex)
import           Error
import           Syntax

data ZAMValue =
  IntVal Int
  | BoolVal Bool
  | ClosVal ZAMCode ZAMEnv
  deriving Show

type ZAMStack = [ZAMValue]
type ZAMEnv = [ZAMValue]

type Address = Int
data ZAMInst =
  Ldi Int     -- 整数をload（スタックに積む）
  | Ldb Bool  -- bool値をload
  | Access Address
  | Closure ZAMCode
  | Apply
  | Return
  | Let
  | EndLet
  | Test ZAMCode ZAMCode
  | BinOp BinOp
  deriving Show

type ZAMCode = [ZAMInst]
type ExecState = (ZAMCode, ZAMStack, ZAMEnv)

transition :: ExecState -> ZAMValue
transition ([], [v], _)             = v
transition (Ldi n : c, st, env)      = transition (c, IntVal n : st, env)
transition (Ldb b : c, st, env)      = transition (c, BoolVal b : st, env)
transition e@(Access i : c, st, env)
  | length env > i = transition (c, env !! i : st, env)
  | otherwise = error ("access error" ++ show e)
transition (Closure c' : c, st, env) = transition (c, ClosVal c' env : st, env)
transition (Apply : c, ClosVal c' env' : v : st, env) =
  transition (c', ClosVal c env : st, v : ClosVal c' env' : env')
transition (Return : _, v : ClosVal c' env' : st, _) =
  transition (c', v : st, env')
transition (Let : c, v : st, env) = transition (c, st, v : env)
transition (EndLet : c, st, _ : env) = transition (c, st, env)
transition (Test c1 c2 : c, BoolVal b : st, env) =
  transition ((if b then c1 else c2) ++ c, st, env)
transition (BinOp Add : c, IntVal n1 : IntVal n2 : st, env) =
  transition (c, IntVal (n1+n2) : st, env)
transition (BinOp Sub : c, IntVal n1 : IntVal n2 : st, env) =
  transition (c, IntVal (n1-n2) : st, env)
transition (BinOp Mul : c, IntVal n1 : IntVal n2 : st, env) =
  transition (c, IntVal (n1*n2) : st, env)
transition (BinOp Eq : c, IntVal n1 : IntVal n2 : st, env) =
  transition (c, BoolVal (n1 == n2) : st, env)
transition e = error ("ZAM transition failed" ++ show e)

type VarList = [String]
type Compile a = ReaderT VarList Check a

compile :: Expr -> Compile ZAMCode
compile (Var x) = do
  env <- ask
  case elemIndex x env of
    Just i -> return [Access i]
    _      -> liftThrow CompileError
compile (Lambda x e) = do
  c <- local ([x,"dummy"] ++) (compile e)
  return [Closure (c ++ [Return])]
compile (App e1 e2) = do
  c1 <- compile e1
  c2 <- compile e2
  return (c2 ++ c1 ++ [Apply])
compile (LetExpr bs body) = do
  let (xs,es) = unzip bs
  cs <- mapM compile es
  cBody <- local (xs ++) (compile body)
  return (concat cs ++ [Let] ++ cBody ++ [EndLet])
compile (LetRecExpr bs body) = do
  let fs = [f | (f,_,_) <- bs]
      recEnv = reverse fs
  cs <- mapM (\(_,x,e) -> local ((x:fs) ++) (compile e)) bs
  cBody <- local (fs ++) (compile body)
  return ([Closure (concat cs ++ [Return]), Let] ++ cBody ++ [EndLet])
compile (LInt n) = return [Ldi n]
compile (LBool b) = return [Ldb b]
compile (BinExpr op e1 e2) = do
  c1 <- compile e1
  c2 <- compile e2
  return (c2 ++ c1 ++ [BinOp op])
compile (IfExpr e1 e2 e3) = do
  c1 <- compile e1
  c2 <- compile e2
  c3 <- compile e3
  return (c1 ++ [Test c2 c3])

type Assign = [(Name, ZAMValue)]

runZAM :: VarList -> Assign -> Expr -> Check ZAMValue
runZAM [] binds e = do
  let (xs,vs) = unzip binds
  code <- runReaderT (compile e) xs
  return (transition (code, [], vs))
runZAM vl binds e = do
  let (xs,vs) = unzip binds
  code <- runReaderT (compile e) (vl ++ xs)
  return $ transition ([Closure (code ++ [Return])], [], vs)

data StmtResult = RVal ZAMValue | RAssign Assign

execZAM :: Assign -> Stmt -> Check StmtResult
execZAM bs (ExprStmt ex) = do
  let (xs,vs) = unzip bs
  cExpr <- runReaderT (compile ex) xs
  let val = transition (cExpr, [], vs)
  return (RVal val)
execZAM assign (LetStmt ebinds) = do
  let (xs,vs) = unzip assign
      (ys,es) = unzip ebinds
  codes <- mapM (\e -> runReaderT (compile e) xs) es
  let vals = [transition (c, [], vs) | c <- codes]
  return (RAssign $ zip ys vals)
execZAM assign (LetRecStmt eBinds) = do
  let (xs,vs) = unzip assign
      (fs,ys,_) = unzip3 eBinds
  codes <- mapM (\(_,y,e) -> runReaderT (compile e) (xs ++ [y] ++ fs)) eBinds
  let vals = [transition ([Closure (c ++ [Return])], [], vs) | c <- codes]
  return (RAssign $ zip ys vals)
