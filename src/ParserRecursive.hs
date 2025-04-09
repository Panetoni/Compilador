-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module ParserRecursive (parseAndPrintAST, parseProgram, AST(..)) where

import Lexer (Token(..), alexScanTokens)

-- Definição da Árvore de Sintaxe para Lang
data AST
  = Node String [AST]
  | Leaf String
  deriving (Show)

-- Função principal para análise sintática de programas Lang
parseProgram :: String -> AST
parseProgram input =
  let tokens = alexScanTokens input
  in Node "Program" (buildAST tokens)

-- Construção da AST com base nos tokens
buildAST :: [Token] -> [AST]
buildAST [] = []
buildAST (ID name : PUNC "(" : rest) =
  let (params, remaining1) = span (/= PUNC ")") rest
      (body, remaining2) = parseBlock (drop 1 remaining1)
  in Node ("Function: " ++ name) (map (Leaf . show) params ++ body) : buildAST remaining2
buildAST (PRINT : rest) =
  let (expr, remaining) = parseExpression rest
  in Node "Print" expr : buildAST remaining
buildAST (IF : rest) =
  let (cond, remaining1) = parseExpression rest
      (thenBranch, remaining2) = parseBlock remaining1
      (elseBranch, remaining3) = if take 2 remaining2 == [ELSE, PUNC "{"]
                                  then parseBlock (drop 1 remaining2)
                                  else ([], remaining2)
  in Node "If" (cond ++ [Node "Then" thenBranch] ++ [Node "Else" elseBranch]) : buildAST remaining3
buildAST (RETURN : rest) =
  let (expr, remaining) = parseExpression rest
  in Node "Return" expr : buildAST remaining
buildAST (PUNC "{" : rest) =
  let (block, remaining) = parseBlock rest
  in [Node "Block" block] ++ buildAST remaining
buildAST (token : rest) = Leaf (show token) : buildAST rest

parseExpression :: [Token] -> ([AST], [Token])
parseExpression (ID name : rest) = ([Leaf ("ID: " ++ name)], rest)
parseExpression (INT val : rest) = ([Leaf ("INT: " ++ show val)], rest)
parseExpression (OP op : rest) = ([Leaf ("OP: " ++ op)], rest)
parseExpression (PUNC p : rest) = ([Leaf ("PUNC: " ++ p)], rest)
parseExpression tokens = ([], tokens)

parseBlock :: [Token] -> ([AST], [Token])
parseBlock (PUNC "{" : rest) =
  let (blockTokens, remaining) = span (/= PUNC "}") rest
  in (buildAST blockTokens, drop 1 remaining)
parseBlock tokens = ([], tokens)

-- Função para exibir a AST formatada corretamente
formatAST :: AST -> String
formatAST ast = unlines (formatHelper ast "")
  where
    formatHelper :: AST -> String -> [String]
    formatHelper (Leaf val) prefix = [prefix ++ "└─ " ++ val]
    formatHelper (Node label children) prefix =
      (prefix ++ "├─ " ++ label) : concatMap (\child -> formatHelper child (prefix ++ "  |")) children

-- Função para processar o conteúdo do arquivo e exibir a AST
parseAndPrintAST :: String -> IO ()
parseAndPrintAST content = do
  let ast = parseProgram content
  putStrLn "\nÁrvore de Sintaxe Gerada:"
  putStrLn (formatAST ast)