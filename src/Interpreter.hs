-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module Interpreter (interpProgram, interpStmt, interpExp, interpBlock) where

import Syntax

type Env = [(String, Value)]

-- Operações matemáticas seguras
interpOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Value -> Value -> Value
interpOp _ floatOp (EFloat n) (EFloat m) = EFloat (floatOp n m)
interpOp intOp _ (EInt n) (EInt m) = EInt (intOp n m)
interpOp _ _ _ _ = error "Type error"

interpExp :: Env -> Exp -> IO Value
interpExp _ (EValue v) = return v
interpExp env (EVar v) = case lookup v env of
  Just val -> return val
  Nothing  -> error $ "Undefined variable: " ++ v
interpExp env (e1 :+: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  return (interpOp (+) (+) v1 v2)
interpExp env (e1 :-: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  return (interpOp (-) (-) v1 v2)
interpExp env (e1 :*: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  return (interpOp (*) (*) v1 v2)
interpExp env (e1 :/: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  case (v1, v2) of
    (_, EInt 0) -> error "Division by zero"
    (_, EFloat 0.0) -> error "Division by zero"
    _ -> return (interpOp div (/) v1 v2)
interpExp env (e1 :==: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  return (EBool (v1 == v2))
interpExp env (e1 :<: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  case (v1, v2) of
    (EInt n, EInt m) -> return (EBool (n < m))
    (EFloat n, EFloat m) -> return (EBool (n < m))
    _ -> error "Type error"
interpExp env (ENot e) = do
  v <- interpExp env e
  case v of
    EBool b -> return (EBool (not b))
    _ -> error "Type error"
interpExp env (e1 :&: e2) = do
  v1 <- interpExp env e1
  v2 <- interpExp env e2
  case (v1, v2) of
    (EBool b1, EBool b2) -> return (EBool (b1 && b2))
    _ -> error "Type error"

interpStmt :: Env -> Stmt -> IO Env
interpStmt env (Assign v e) = do
  val <- interpExp env e
  return ((v, val) : env)
interpStmt env (Print e) = do
  val <- interpExp env e
  print val
  return env
interpStmt env (Return _) = return env
interpStmt env (If e b1 b2) = do
  val <- interpExp env e
  case val of
    EBool True -> interpBlock env b1
    EBool False -> interpBlock env b2
    _ -> error "Type error"
interpStmt env (While cond body) = loop env
  where
    loop env' = do
      val <- interpExp env' cond
      case val of
        EBool True -> interpBlock env' body >>= loop
        EBool False -> return env'
        _ -> error "Type error"
interpStmt env Skip = return env
interpStmt env (FunCall _ _) = return env  -- Placeholder para futuras implementações

interpBlock :: Env -> Block -> IO Env
interpBlock env (Block stmts) = foldl (\acc stmt -> acc >>= \env' -> interpStmt env' stmt) (return env) stmts

interpProgram :: Program -> IO Env
interpProgram (Program _) = do
  putStrLn "\nPrograma interpretado."
  return []
