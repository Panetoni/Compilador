-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module Syntax where

import Parser (AST(..))

-- Definição de expressões

data Value
  = EInt Int
  | EFloat Float
  | EBool Bool
  | EChar Char
  deriving (Eq, Ord, Show)

data Exp
  = EValue Value
  | EVar String
  | Exp :+: Exp
  | Exp :-: Exp
  | Exp :*: Exp
  | Exp :/: Exp
  | Exp :==: Exp
  | Exp :<: Exp
  | ENot Exp
  | Exp :&: Exp
  deriving (Eq, Ord, Show)

-- Tipos suportados

data Ty = TInt | TFloat | TBool | TChar deriving (Eq, Ord, Show)

-- Estrutura de um programa em Lang

data Program = Program [Decl] deriving (Eq, Ord, Show)

data Decl
  = FunDecl String [(String, Ty)] [Ty] Block
  deriving (Eq, Ord, Show)

newtype Block = Block [Stmt] deriving (Eq, Ord, Show)

data Stmt
  = Skip
  | Assign String Exp
  | If Exp Block Block
  | Print Exp
  | Return [Exp]
  | While Exp Block
  | FunCall String [Exp]
  deriving (Eq, Ord, Show)

-- Conversão de AST do Parser para a Estrutura de Program

astToProgram :: AST -> Program
astToProgram (ProgramStmtList decls) = Program (map astToDecl decls)
astToProgram _ = error "Erro: AST não representa um programa válido"

astToDecl :: AST -> Decl
astToDecl (FunStmt name params retType body) = FunDecl name (map (\(p, t) -> (p, strToTy t)) params) (map strToTy retType) (astToBlock body)
astToDecl _ = error "Erro: Declaração inválida na AST"

astToBlock :: AST -> Block
astToBlock (BlockStmt stmts) = Block (map astToStmt stmts)
astToBlock _ = error "Erro: Bloco inválido"

astToStmt :: AST -> Stmt
astToStmt (PrintStmt expr) = Print (astToExp expr)
astToStmt (ReturnStmt exprs) = Return (map astToExp exprs)
astToStmt _ = error "Erro: Comando inválido na AST"

astToExp :: AST -> Exp
astToExp (VarExpr name) = EVar name
astToExp (IntExpr n) = EValue (EInt n)
astToExp (FloatExpr n) = EValue (EFloat n)
astToExp (BoolExpr b) = EValue (EBool b)
astToExp (CharExpr c) = EValue (EChar c)
astToExp _ = error "Erro: Expressão inválida"

-- Conversão de String para Tipo
strToTy :: String -> Ty
strToTy "Int" = TInt
strToTy "Float" = TFloat
strToTy "Bool" = TBool
strToTy "Char" = TChar
strToTy _ = error "Erro: Tipo desconhecido"


pprintExp :: Exp -> String 
pprintExp (e1 :+: e2)
  = unwords [ pprintExp e1
            , "+"
            , pprintExp e2]
pprintExp other 
  = pprintTerm other 


pprintTerm :: Exp -> String 
pprintTerm (e1 :*: e2)
  = unwords [ pprintTerm e1
            , "*"
            , pprintTerm e2
            ]
pprintTerm other 
  = pprintFactor other 

pprintFactor :: Exp -> String 
pprintFactor (n)
  = show n
pprintFactor other 
  = unwords [ "("
            , pprintExp other 
            , ")"]
