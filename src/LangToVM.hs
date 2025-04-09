-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module LangToVM (convertLangToVM) where

import Lexer (alexScanTokens)
import Parser (parser, AST(..))
import System.IO ()
import System.FilePath (replaceExtension)

convertLangToVM :: FilePath -> IO ()
convertLangToVM fileName = do
    content <- readFile fileName
    let tokens = alexScanTokens content
    let ast = parser tokens
    let vmCode = case ast of
            MainWithDecls bloco decls -> transpileBlock bloco ++ "\n" ++ unlines (map transpileDecl decls) ++ "\nHALT\n"
            MainProgram bloco -> transpileBlock bloco ++ "\nHALT\n"
            ProgramStmtList decls -> unlines (map transpileDecl decls) ++ "\nHALT\n"
            _ -> error "Estrutura de programa inválida para conversão"
    let outputFileName = replaceExtension fileName ".vm"
    writeFile outputFileName vmCode
    putStrLn $ "Código convertido para VM salvo em " ++ outputFileName

transpileBlock :: AST -> String
transpileBlock (BlockStmt stmts) = unlines (map transpileStmt stmts)
transpileBlock _ = ""

transpileDecl :: AST -> String
transpileDecl (FunStmt nome _ _ corpo) = nome ++ ":\n" ++ transpileBlock corpo ++ "\nRET"
transpileDecl _ = ""

transpileStmt :: AST -> String
transpileStmt (PrintStmt expr) = transpileExpr expr ++ "\nOUT"
transpileStmt (ReturnStmt exprs) = unlines (map transpileExpr exprs) ++ "\nRET"
transpileStmt (OpExpr "+" e1 e2) = transpileExpr e1 ++ "\n" ++ transpileExpr e2 ++ "\nADD"
transpileStmt (OpExpr "*" e1 e2) = transpileExpr e1 ++ "\n" ++ transpileExpr e2 ++ "\nMUL"
transpileStmt (CallStmt nome args) = unlines (map transpileExpr args) ++ "\nCALL " ++ nome
transpileStmt (AssignmentStmt (LValueAssignment (VarExpr var) expr)) = transpileExpr expr ++ "\nSTORE " ++ var
transpileStmt _ = ""

transpileExpr :: AST -> String
transpileExpr (IntExpr n) = "PUSHI " ++ show n
transpileExpr (VarExpr s) = "PUSH " ++ s
transpileExpr _ = ""