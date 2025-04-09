-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016
{
module Lexer (Token(..), lexer, alexScanTokens) where

}
%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z_]
$alnum = [a-zA-Z0-9_]

tokens :-
  [ \t\n\r]+               ;  -- Ignorar espaços e quebras de linha
  $alpha($alnum)* { \s -> mkIdent s }  -- Identificador ou palavra-chave
  $digit+        { \s -> INT (read s) }  -- Número inteiro
  $digit+"."$digit+ { \s -> FLOAT (read s) }  -- Número float
  "'[^\']'"      { \s -> case s of
                          ('\'':c:'\'':[]) -> CHAR c
                          _ -> error ("Invalid char literal: " ++ s) }
  "&&"           { \_ -> OP "&&" }
  "=="           { \_ -> OP "==" }
  "!="           { \_ -> OP "!=" }
  "<="           { \_ -> OP "<=" }
  ">="           { \_ -> OP ">=" }
  "<"            { \_ -> OP "<" }
  ">"            { \_ -> OP ">" }
  "\\+"          { \_ -> OP "+" }
  "-"            { \_ -> OP "-" }
  "\\*"          { \_ -> OP "*" }
  "/"            { \_ -> OP "/" }
  "*"            { \_ -> OP "*" }
  "%"            { \_ -> OP "%" }
  "!"            { \_ -> OP "!" }
  "+"            { \_ -> OP "+" }
  "="            { \_ -> ASSIGN }
  "::"           { \_ -> PUNC "::" }
  ";"            { \_ -> PUNC ";" }
  ","            { \_ -> PUNC "," }
  "("            { \_ -> PUNC "(" }
  ")"            { \_ -> PUNC ")" }
  "{"            { \_ -> PUNC "{" }
  "}"            { \_ -> PUNC "}" }
  "["            { \_ -> PUNC "[" }
  "]"            { \_ -> PUNC "]" }
  "."            { \_ -> PUNC "." }
  " "            ;
  ":"            { \_ -> PUNC ":" }
  .              { \s -> error $ "Erro lexical: caractere inesperado '" ++ s ++ "'" }

{
data Token
  = ID String
  | TYPEID String
  | INT Int
  | FLOAT Float
  | CHAR Char
  | BOOL Bool
  | NULL
  | PRINT
  | IF
  | ELSE
  | THEN
  | RETURN
  | FUN
  | DATA
  | MAIN
  | ITERATE
  | READ
  | NEW
  | OP String
  | PUNC String
  | ASSIGN
  | WHILE
  | BREAK
  deriving (Show, Eq)

lexer :: String -> [Token]
lexer = alexScanTokens


-- Função para distinguir palavras-chave de identificadores
mkIdent :: String -> Token
mkIdent s = case s of
    "if"      -> IF
    "then"    -> THEN
    "else"    -> ELSE
    "true"    -> BOOL True
    "false"   -> BOOL False
    "print"   -> PRINT
    "read"    -> READ
    "while"   -> WHILE
    "return"  -> RETURN
    "fun"     -> FUN
    "data"    -> DATA
    "iterate" -> ITERATE
    "null"    -> NULL
    "new"     -> NEW
    "break"   -> BREAK
    _         -> ID s
}
