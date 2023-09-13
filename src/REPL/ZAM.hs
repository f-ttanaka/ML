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

type REPL a = StateT (Binds, TEnv) IO ()

evalStmt :: Stmt -> REPL ()
evalStmt (ExprStmt ex) = do
  (binds,tEnv) <- get
  case runInfer tEnv ex of
    Left err -> lift (print err)
    _        -> case runZAM [] binds ex of
      Left err  -> lift (print err)
      Right res -> lift (putStrLn $ show ex ++ " = " ++ show res)
evalStmt (LetStmt bs) = do
  (binds, tEnv) <- get
  let (xs,es) = unzip bs
  case runInferBinds tEnv bs of
    Left err -> lift (print err)
    Right bs' -> case mapM (runZAM [] binds) es of
      Left err -> lift (print err)
      Right vs ->
        let binds' = binds ++ zip xs vs
            tEnv' = bindVars bs' tEnv
            output = unlines [x ++ " : " ++ show t | (x,t) <- bs']
        in put (binds', tEnv') >> lift (putStr output)
evalStmt (LetRecStmt bs) = do
  (binds,tEnv) <- get
  case runInferRecBinds tEnv bs of
    Left err -> lift (print err)
    Right bs' -> do
      let output = unlines [f ++ " : " ++ show t | (f,t) <- bs']
          (fs,_,_) = unzip3 bs
      case mapM (\(_,x,e) -> runZAM (x:fs) binds e) bs of
        Left err -> lift (print err)
        Right vs ->
          let binds' = binds ++ zip fs vs
          in put (binds', bindVars bs' tEnv) >> lift (putStr output)

readEvalFile :: FilePath -> REPL ()
readEvalFile fp = do
  str <- lift (readFile fp)
  case parseFile str of
    Left err   -> lift (print err)
    Right stms -> mapM_ evalStmt stms

loop :: REPL ()
loop = do
  input <- lift (readPrompt "zam>> ")
  case parseCommand input of
    Right (ComEvalStmt stm) -> evalStmt stm >> loop
    Right (ComEvalFile fp)  -> readEvalFile fp >> loop
    Right Quit              -> return ()
    Left err                -> lift (print err) >> loop

runREPL :: IO ()
runREPL = evalStateT loop mempty
