-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

module LangToPython (convertLangToPython) where

import Lexer (alexScanTokens)
import Parser (parser, AST(..))
import System.IO (writeFile)
import System.FilePath (replaceExtension)

convertLangToPython :: FilePath -> IO ()
convertLangToPython fileName = do
    content <- readFile fileName
    let tokens = alexScanTokens content
    let ast = parser tokens
    let pythonCode = transpileAST ast ++ "\nmain()\n"
    let outputFileName = replaceExtension fileName ".py"
    writeFile outputFileName pythonCode
    putStrLn $ "Código convertido para Python salvo em " ++ outputFileName

transpileAST :: AST -> String
transpileAST (MainWithDecls bloco decls) =
    let declsPython = unlines (map transpileDecl decls)
        mainPython = "\ndef main():\n" ++ transpileBlock bloco 1 ++ "\n"
        mainCall = "\nif __name__ == \"__main__\":\n    main()\n"
    in declsPython ++ "\n" ++ mainPython ++ mainCall
transpileAST (MainProgram bloco) =
    "\ndef main():\n" ++ transpileBlock bloco 1 ++ "\n" ++ "\nif __name__ == \"__main__\":\n    main()\n"
transpileAST (ProgramStmtList decls) = unlines (map transpileDecl decls)
transpileAST _ = error "Estrutura de programa inválida para conversão"

transpileDecl :: AST -> String
transpileDecl (FunStmt nome params ret corpo) =
    let paramsStr = unwords (map fst params)
        paramsFormatted = if null params then "" else unwords $ map (++ ",") (init (words paramsStr)) ++ [last (words paramsStr)]
    in "def " ++ nome ++ "(" ++ paramsFormatted ++ "):\n" ++ transpileBlock corpo 1
transpileDecl _ = ""

transpileBlock :: AST -> Int -> String
transpileBlock (BlockStmt stmts) indentLevel =
    let indent = replicate (indentLevel * 4) ' '
    in if null stmts then indent ++ "pass\n" else unlines (map (\stmt -> indent ++ transpileStmt stmt indentLevel) stmts)
transpileBlock _ _ = "    pass\n"  -- Se o bloco estiver vazio, colocar 'pass'

transpileStmt :: AST -> Int -> String
transpileStmt (PrintStmt expr) _ = "print(" ++ transpileExpr expr ++ ")"
transpileStmt (ReturnStmt exprs) _ = "return " ++ transpileReturn exprs
transpileStmt (IfStmt cond thenBloco elseBloco) indentLevel =
    "if " ++ transpileExpr cond ++ ":\n" ++ transpileBlock thenBloco (indentLevel + 1) ++
    (case elseBloco of
        BlockStmt [] -> ""
        _ -> replicate (indentLevel * 4) ' ' ++ "else:\n" ++ transpileBlock elseBloco (indentLevel + 1))
transpileStmt (WhileStmt cond bloco) indentLevel =
    "while " ++ transpileExpr cond ++ ":\n" ++ transpileBlock bloco (indentLevel + 1)
transpileStmt (CallStmt nome args) _ = nome ++ "(" ++ transpileArgs args ++ ")"
transpileStmt (AssignmentStmt (LValueAssignment (IndexExpr (VarExpr var) index) expr)) _ =
    var ++ "[" ++ transpileExpr index ++ "] = " ++ transpileExpr expr
transpileStmt (AssignmentStmt (LValueAssignment (VarExpr var) expr)) _ =
    var ++ " = " ++ transpileExpr expr
transpileStmt _ _ = "pass"

transpileArgs :: [AST] -> String
transpileArgs [] = ""
transpileArgs args = unwords $ map (++ ",") (init (map transpileExpr args)) ++ [transpileExpr (last args)]

transpileExpr :: AST -> String
transpileExpr (VarExpr s) = s
transpileExpr (IntExpr n) = show n
transpileExpr (FloatExpr f) = show f
transpileExpr (CharExpr c) = show c
transpileExpr (BoolExpr True) = "True"
transpileExpr (BoolExpr False) = "False"
transpileExpr (OpExpr "&&" e1 e2) = transpileExpr e1 ++ " and " ++ transpileExpr e2
transpileExpr (OpExpr "||" e1 e2) = transpileExpr e1 ++ " or " ++ transpileExpr e2
transpileExpr (OpExpr "!" e1 e2) = "not " ++ transpileExpr e1
transpileExpr (OpExpr op e1 e2) = transpileExpr e1 ++ " " ++ op ++ " " ++ transpileExpr e2
transpileExpr (CallExpr nome args) = nome ++ "(" ++ transpileArgs args ++ ")"
transpileExpr (IndexExpr expr (IntExpr 0)) = transpileExpr expr 
transpileExpr (IndexExpr expr index) = transpileExpr expr ++ "[" ++ transpileExpr index ++ "]"
transpileExpr (SliceExpr arr start end) = transpileExpr arr ++ "[" ++ transpileExpr start ++ ":" ++ transpileExpr end ++ "]"
transpileExpr (ArrayLiteral elems) = "[" ++ transpileArgs elems ++ "]"
transpileExpr _ = "" 

transpileReturn :: [AST] -> String
transpileReturn exprs = if length exprs > 1 then "(" ++ transpileArgs exprs ++ ")" else transpileArgs exprs