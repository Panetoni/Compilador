-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module PEG (parsePEGAndInterpret) where

import Lexer (alexScanTokens)
import Parser (parser)
import Syntax (astToProgram)
import Interpreter (interpProgram)

-- Função para analisar usando o modo Recursive e interpretar a AST
parsePEGAndInterpret :: String -> IO ()
parsePEGAndInterpret input = do
  let tokens = alexScanTokens input
  let ast = parser tokens
  let program = astToProgram ast
  putStrLn "\nExecutando análise PEG (Recursive) e interpretando..."
  _ <- interpProgram program
  return ()