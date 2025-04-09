-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

-- Para compilar: stack build
-- Para executar: stack exec -- lang-compiler-exe --<opção> test/<arquivo.lang>
-- Exemplo: stack exec -- lang-compiler-exe --lexer test/exemplo.lang

module Main (main) where

import Lexer (alexScanTokens)
import Parser (parser, prettyPrintAST)
import PEG (parsePEGAndInterpret)
import LangToPython (convertLangToPython)
import LangToVM (convertLangToVM)
import ParserRecursive (parseAndPrintAST)
import Semantic (analyze)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lexer", fileName] -> do
      content <- readFile fileName
      let tokens = alexScanTokens content
      putStrLn "Tokens gerados:"
      mapM_ print tokens

    ["--recursive-tree", fileName] -> do
      content <- readFile fileName
      putStrLn "\nAnálise Recursiva e Impressão da AST:"
      parseAndPrintAST content

    ["--peg", fileName] -> do
      content <- readFile fileName
      putStrLn "\nAnálise PEG e execução:"
      parsePEGAndInterpret content

    ["--lalr", fileName] -> do
      content <- readFile fileName
      let tokens = alexScanTokens content
      let ast = parser tokens
      putStrLn "\nAnálise LALR e execução"
      putStrLn "\nÁrvore de Sintaxe Abstrata (AST) com LALR:"
      putStrLn (prettyPrintAST ast)

    ["--typed-python", fileName] -> do
      content <- readFile fileName
      result <- analyze content
      case result of
        Left errors -> do
          putStrLn "\nErros Semânticos encontrados:"
          mapM_ print errors
        Right _ -> do
          putStrLn "\nAnálise semântica concluída sem erros."
          convertLangToPython fileName

    ["--typed-vm", fileName] -> do
      content <- readFile fileName
      result <- analyze content
      case result of
        Left errors -> do
          putStrLn "\nErros Semânticos encontrados:"
          mapM_ print errors
        Right _ -> do
          putStrLn "\nAnálise semântica concluída sem erros."
          convertLangToVM fileName      
      
    ["--typed-interp", fileName] -> do
      content <- readFile fileName
      result <- analyze content
      case result of
        Left errors -> do
          putStrLn "\nErros Semânticos encontrados:"
          mapM_ print errors
        Right _ -> putStrLn "\nAnálise semântica concluída sem erros."

    ["--help"] -> do
      putStrLn "  Lexer           - Processa o arquivo de entrada e exibe os tokens."
      putStrLn "  Recursive Tree  - Processa o arquivo de entrada e exibe a árvore de sintaxe."
      putStrLn "  LALR            - Realiza a análise sintática da linguagem Lang e gera a AST."
      putStrLn "  PEG             - Executa o código representado pela árvore de sintaxe produzida pelo parser usando o interpretador."
      putStrLn "  typed-python    - Converte o código Lang para Python tipado."
      putStrLn "  typed-interp    - Realiza a análise semântica do código-fonte."
      putStrLn "  typed-vm        - Converte o código Lang para o código de máquina virtual."
      putStrLn "  Help            - Mostra esta mensagem de ajuda."

    _ -> putStrLn "Uso correto: stack exec lang-compiler-exe --<opção> <arquivo.lang>\nOpções: --lexer, --recursive-tree, --peg, --lalr, --typed-python, --typed-interp, --typed-vm"
