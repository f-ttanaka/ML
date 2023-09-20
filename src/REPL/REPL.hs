module REPL.REPL (runREPL) where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State
import           Env
import           Eval.TreeWalk
import           Parser.Parser             (parseCommand, parseFile, parseStmt)
import           REPL.Prompt
import           Syntax
import           Typing.Infer

type REPL a = StateT (VEnv,TEnv) IO a

evalStmt :: Stmt -> REPL ()
evalStmt (ExprStmt ex) = do
  (vEnv,tEnv) <- get
  case (runInfer tEnv ex, runEval vEnv ex) of
    (Left err, _)  -> lift (print err)
    (_, Left err)  -> lift (print err)
    (_, Right val) -> lift $ putStrLn (show ex ++ " = " ++ show val)
evalStmt (LetStmt binds) = do
  (vEnv,tEnv) <- get
  let (xs,es) = unzip binds
  case (runInferBinds tEnv binds, mapM (runEval vEnv) es) of
    (Left err, _) -> lift (print err)
    (_, Left err) -> lift (print err)
    (Right tBinds, Right vs) ->
      let tEnv' = bindVars tBinds tEnv
          vEnv' = bindVars (zip xs vs) vEnv
          output = unlines [x ++ " : " ++ show t | (x,t) <- tBinds]
      in put (vEnv', tEnv') >> lift (putStr output)
evalStmt (LetRecStmt binds) = do
  (vEnv,tEnv) <- get
  case runInferRecBinds tEnv binds of
    Left err -> lift (print err)
    Right tBinds ->
      let output = unlines [f ++ " : " ++ show t | (f,t) <- tBinds]
          recEnv = bindVars [(f, VClosure x e recEnv) | (f,x,e) <- binds] vEnv
      in put (recEnv, bindVars tBinds tEnv) >> lift (putStr output)

readEvalFile :: FilePath -> REPL ()
readEvalFile fp = do
  str <- lift (readFile fp)
  case parseFile str of
    Left err   -> lift (print err)
    Right stms -> mapM_ evalStmt stms

loop :: REPL ()
loop = do
  input <- lift (readPrompt ">> ")
  case parseCommand input of
    Right (ComEvalStmt stm) -> evalStmt stm >> loop
    Right (ComEvalFile fp)  -> readEvalFile fp >> loop
    Right Quit              -> return ()
    Left err                -> lift (print err) >> loop

runREPL :: IO ()
runREPL = evalStateT loop mempty
