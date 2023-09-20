module REPL.ZAM where

import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.State
import           Env
import           Error
import           Eval.ZAM.Compile
import           Parser.Parser             (parseCommand, parseExpr, parseFile,
                                            parseStmt)
import           REPL.Prompt
import           Syntax
import           Typing.Infer

type REPL a = StateT (Assign, TEnv) IO ()

checkStmt :: Stmt -> REPL ()
checkStmt (ExprStmt ex) = do
  (_,tEnv) <- get
  case runInfer tEnv ex of
    Left err -> lift (print err)
    _        -> return ()
checkStmt (LetStmt binds) = do
  (assign, tEnv) <- get
  case runInferBinds tEnv binds of
    Left err -> lift (print err)
    Right bs' ->
      let tEnv' = bindVars bs' tEnv
          output = unlines [x ++ " : " ++ show t | (x,t) <- bs']
      in put (assign, tEnv') >> lift (putStr output)
checkStmt (LetRecStmt binds) = do
  (assign,tEnv) <- get
  case runInferRecBinds tEnv binds of
    Left err -> lift (print err)
    Right bs' ->
      let output = unlines [f ++ " : " ++ show t | (f,t) <- bs']
      in put (assign, bindVars bs' tEnv) >> lift (putStr output)

evalStmt :: Stmt -> REPL ()
evalStmt stmt = do
  (assign,tEnv) <- get
  case execZAM assign stmt of
    Left err           -> lift (print err)
    Right (RVal rv)    -> lift (print rv)
    Right (RAssign ra) -> put (ra ++ assign, tEnv)

readEvalFile :: FilePath -> REPL ()
readEvalFile fp = do
  str <- lift (readFile fp)
  case parseFile str of
    Left err   -> lift (print err)
    Right stms -> mapM_ (\stm -> checkStmt stm >> evalStmt stm) stms

loop :: REPL ()
loop = do
  input <- lift (readPrompt "zam>> ")
  case parseCommand input of
    Right (ComEvalStmt stm) -> checkStmt stm >> evalStmt stm >> loop
    Right (ComEvalFile fp)  -> readEvalFile fp >> loop
    Right Quit              -> return ()
    Left err                -> lift (print err) >> loop

runREPL :: IO ()
runREPL = evalStateT loop mempty
