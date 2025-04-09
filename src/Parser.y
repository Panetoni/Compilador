-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

{
-- Cabeçalho do módulo e importação do Lexer
module Parser where
import Lexer
}

--------------------------------------------------------------------------------
-- Declarações e Configurações do Parser
-- Nome do parser, mensagens de erro, tipo de token, tokens (pseudo-terminais)
-- e precedências (associatividade) – todos organizados em ordem alfabética.
--------------------------------------------------------------------------------

%name parser Program
%error { parseError }
%tokentype { Token }

-- Tokens (pseudo-terminais) – em ordem alfabética
%token
    ASSIGN        { ASSIGN }
    BREAK         { BREAK }
    BOOL          { BOOL $$ }
    CHAR          { CHAR $$ }
    DATA          { DATA }
    DOT           { PUNC "." }
    ELSE          { ELSE }
    FLOAT         { FLOAT $$ }
    FUN           { FUN }
    ID            { ID $$ }
    IF            { IF }
    INT           { INT $$ }
    ITERATE       { ITERATE }
    MAIN          { MAIN }
    NEW           { NEW }
    NULL          { NULL }
    OP_GE         { OP ">=" }
    OP_LE         { OP "<=" }
    LT            { OP "<" }
    RT            { OP ">" }
    PLUS          { OP "+" }
    MINUS         { OP "-" }
    AND           { OP "&&" }
    OP            { OP $$ }
    P_COLON       { PUNC ":" }
    P_COMMA       { PUNC "," }
    P_DOUBLECOLON { PUNC "::" }
    P_LBRACE      { PUNC "{" }
    P_LBRACKET    { PUNC "[" }
    P_LPAREN      { PUNC "(" }
    P_RBRACE      { PUNC "}" }
    P_RBRACKET    { PUNC "]" }
    P_RPAREN      { PUNC ")" }
    P_SEMICOLON   { PUNC ";" }
    PRINT         { PRINT }      -- token PRINT reinserido conforme o código original
    READ          { READ }
    RETURN        { RETURN }
    TYPEID        { TYPEID $$ }
    WHILE         { WHILE }      -- token para while

-- Precedência e associatividade – em ordem alfabética

%left AND 
%left OP_LE OP_GE LT RT                 
%left PLUS MINUS
%left OP
%left DOT


%nonassoc P_LBRACKET P_RBRACKET
%nonassoc RETURN
%right ELSE

--------------------------------------------------------------------------------
-- Regras Gramaticais
-- As produções estão organizadas em ordem alfabética pelo não-terminal à esquerda.
--------------------------------------------------------------------------------

%%

-- ArgumentList: Lista de argumentos para chamadas de função
ArgumentList
    : Expression                                   { [$1] }
    | ArgumentList P_COMMA Expression              { $1 ++ [$3] }
    | {- empty -}                                  { [] }

-- Assignment: Atribuição simples
Assignment
    : LValue ASSIGN Expression                     { LValueAssignment $1 $3 }

-- Block: Bloco de comandos delimitado por chaves
Block
    : P_LBRACE StatementList P_RBRACE              { BlockStmt $2 }

-- Declaration: Declaração de função (identificador, parâmetros, tipo de retorno e corpo)
Declaration
    : ID P_LPAREN ParamListWithTypes P_RPAREN ReturnTy Block  
                                                  { FunStmt $1 $3 $5 $6 }

-- DeclarationList: Lista de declarações (funções etc.)
DeclarationList
    : Declaration                                  { [$1] }
    | Declaration DeclarationList                  { $1 : $2 }

-- Expression: Expressões da linguagem
Expression
    : ID                                           { VarExpr $1 }
    | INT                                          { IntExpr $1 }
    | FLOAT                                        { FloatExpr $1 }
    | CHAR                                         { CharExpr $1 }
    | BOOL                                         { BoolExpr $1 }
    | NULL                                         { NullExpr }

    | Expression PLUS Expression                   { OpExpr "+" $1 $3 }
    | Expression MINUS Expression                  { OpExpr "-" $1 $3 }
    | Expression OP Expression %prec MINUS         { OpExpr $2 $1 $3 } 
    | Expression OP_LE Expression                  { OpExpr "<=" $1 $3 }
    | Expression OP_GE Expression                  { OpExpr ">=" $1 $3 }
    | Expression LT Expression                     { OpExpr "<" $1 $3 }
    | Expression RT Expression                     { OpExpr ">" $1 $3 }
    | Expression AND Expression                    { OpExpr "&&" $1 $3 }


    | ID P_LPAREN ArgumentList P_RPAREN            { CallExpr $1 $3 }
    | NEW NewType                                  { NewExpr $2 }
    | NEW NewType P_LPAREN ArgumentList P_RPAREN   { NewExprArgs $2 $4 }
    | Expression DOT ID                            { FieldAccessExpr $1 $3 }
    | Expression P_LBRACKET Expression P_COLON Expression P_RBRACKET
                                                  { SliceExpr $1 $3 $5 }
    | Expression P_LBRACKET IndexContent P_RBRACKET { IndexExpr $1 $3 }
    | P_LBRACE FieldList P_RBRACE                  { RecordExpr $2 }
    | P_LBRACKET ExpressionList P_RBRACKET         { ArrayLiteral $2 }
    | P_LBRACKET P_RBRACKET                        { ArrayLiteral [] }

-- ExpressionList: Lista de expressões separadas por vírgula
ExpressionList
    : Expression                                   { [$1] }
    | ExpressionList P_COMMA Expression            { $1 ++ [$3] }

-- FieldAssignment: Atribuição de valor a um campo em um registro
FieldAssignment
    : ID P_COLON Expression                        { ($1, $3) }

-- FieldList: Lista de atribuições de campos (em registros)
FieldList
    : FieldAssignment                              { [$1] }
    | FieldAssignment P_COMMA FieldList            { $1 : $3 }
    | {- empty -}                                  { [] }

-- IfCondition: Condição para estruturas if (com ou sem parênteses)
IfCondition
    : P_LPAREN Expression P_RPAREN                 { $2 }
    | Expression                                   { $1 }

-- IndexContent: Conteúdo usado em indexação
IndexContent
    : Expression                                   { $1 }

-- LValue: Identificador (ou acesso a campo/index) para atribuição
LValue
    : ID                                           { VarExpr $1 }
    | LValue DOT ID                                { FieldAccessExpr $1 $3 }
    | LValue P_LBRACKET Expression P_RBRACKET      { IndexExpr $1 $3 }

-- MatchedStmt: Instruções completas (ex.: if-then-else, atribuições com ;, etc.)
MatchedStmt
    : PRINT Expression P_SEMICOLON                 { PrintStmt $2 }
    | IF IfCondition MatchedStmt ELSE MatchedStmt    { IfStmt $2 $3 $5 }
    | RETURN ExpressionList OptionalSemicolon      { ReturnStmt $2 }
    | FUN ID ParamListWithTypes P_COLON TypeList Block  
                                                  { FunStmt $2 $3 $5 $6 }
    | ID ParamListWithTypes P_COLON TypeList Block   { FunStmt $1 $2 $4 $5 }
    | DATA ID Block                                { DataStmt $2 $3 }
    | ITERATE Expression Block                     { IterateStmt $2 $3 }
    | READ ID                                      { ReadStmt $2 }
    | NEW TYPEID P_LPAREN ParamList P_RPAREN Block   { NewStmt $2 $4 $6 }
    | Assignment P_SEMICOLON                         { AssignmentStmt $1 }
    | Block                                        { $1 }
    | ID P_LPAREN ArgumentList P_RPAREN P_SEMICOLON  { CallStmt $1 $3 }
    | BREAK P_SEMICOLON                              { BreakStmt }

-- NewType: Tipo para criação de novo objeto (pode ser TYPEID ou ID)
NewType
    : TYPEID                                       { $1 }
    | ID                                           { $1 }

-- OptionalSemicolon: Ponto-e-vírgula opcional ao final de instruções
OptionalSemicolon
    : P_SEMICOLON                                  { () }
    | {- empty -}                                  { () }

-- ParamList: Lista de parâmetros (sem tipos)
ParamList
    : ID                                           { [$1] }
    | ParamList P_COMMA ID                         { $1 ++ [$3] }
    | {- empty -}                                  { [] }

-- ParamListWithTypes: Lista de parâmetros com tipos (ou vazia)
ParamListWithTypes
    : {- empty -}                                  { [] }
    | ParamWithOrWithoutType                       { [$1] }
    | ParamWithOrWithoutType P_COMMA ParamListWithTypes  
                                                  { $1 : $3 }

-- ParamWithOrWithoutType: Parâmetro com ou sem especificação de tipo
ParamWithOrWithoutType
    : ID                                           { ( $1, "" ) }
    | ID P_DOUBLECOLON Type                        { ( $1, $3 ) }

-- Program: Regra principal do programa
Program
    : MAIN P_LPAREN P_RPAREN Block DeclarationList { MainWithDecls $4 $5 }
    | MAIN P_LPAREN P_RPAREN Block                 { MainProgram $4 }
    | DeclarationList                              { ProgramStmtList $1 }

-- ReturnTy: Tipo de retorno de funções (opcional)
ReturnTy
    : P_COLON TypeList                             { $2 }
    | {- empty -}                                  { [] }

-- Statement: Uma instrução (pode ser while, matched ou unmatched)
Statement
    : WhileStmt                                    { $1 }
    | MatchedStmt                                  { $1 }
    | UnmatchedStmt                                { $1 }

-- StatementList: Lista de instruções (dentro de um bloco)
StatementList
    : Statement                                    { [$1] }
    | StatementList Statement                      { $1 ++ [$2] }

-- Type: Definição de tipo (simples ou array)
Type
    : TYPEID                                       { $1 }
    | ID                                           { $1 }
    | TYPEID P_LBRACKET P_RBRACKET                 { $1 ++ "[]" }
    | ID P_LBRACKET P_RBRACKET                     { $1 ++ "[]" }

-- TypeList: Lista de tipos separados por vírgula
TypeList
    : Type                                         { [$1] }
    | TypeList P_COMMA Type                        { $1 ++ [$3] }

-- UnmatchedStmt: Instrução if sem else (dangling else)
UnmatchedStmt
    : IF IfCondition Statement                     { IfStmt $2 $3 (BlockStmt []) }
    | IF IfCondition MatchedStmt ELSE UnmatchedStmt  { IfStmt $2 $3 $5 }

-- WhileStmt: Instrução while – o corpo deve ser um MatchedStmt
WhileStmt
    : WHILE P_LPAREN Expression P_RPAREN Block { WhileStmt $3 $5 }

--------------------------------------------------------------------------------
-- Definições de AST e Funções Auxiliares
-- Contém a definição da AST, a função de erro e a função para imprimir a AST.
--------------------------------------------------------------------------------

{
-- Definição da Árvore de Sintaxe Abstrata (AST)
-- Construtores organizados em ordem alfabética.
data AST
    = ArrayLiteral [AST]                              -- Lista literal de elementos
    | AssignmentStmt AST                              -- Instrução de atribuição
    | BlockStmt [AST]                                 -- Bloco de instruções
    | BoolExpr Bool                                   -- Expressão booleana
    | BreakStmt                                       -- Instrução break
    | CallExpr String [AST]                           -- Chamada de função (expressão)
    | CallStmt String [AST]                           -- Chamada de função (instrução)
    | CharExpr Char                                   -- Expressão de caractere
    | DataStmt String AST                             -- Declaração de dado
    | FloatExpr Float                                 -- Expressão de ponto flutuante
    | FieldAccessExpr AST String                      -- Acesso a campo de registro
    | FunStmt String [(String, String)] [String] AST   -- Declaração de função
    | IfStmt AST AST AST                              -- Instrução if (condição, then, else)
    | IndexExpr AST AST                               -- Expressão de indexação
    | IntExpr Int                                     -- Expressão inteira
    | IterateStmt AST AST                             -- Instrução iterate
    | LValueAssignment AST AST                        -- Atribuição via lvalue
    | MainProgram AST                                 -- Programa principal sem declarações
    | MainWithDecls AST [AST]                         -- Programa principal com declarações
    | MultipleAssignment String String                -- Atribuição múltipla
    | NewExpr String                                  -- Criação de objeto (sem argumentos)
    | NewExprArgs String [AST]                        -- Criação de objeto (com argumentos)
    | NewStmt String [String] AST                     -- Declaração "new" (tipo, parâmetros, bloco)
    | NullExpr                                        -- Expressão null
    | OpExpr String AST AST                           -- Expressão com operador binário
    | PrintStmt AST                                   -- Instrução de impressão
    | ProgramStmtList [AST]                           -- Lista de instruções do programa
    | ReadStmt String                                 -- Instrução de leitura
    | RecordExpr [(String, AST)]                      -- Expressão de registro
    | ReturnStmt [AST]                                -- Instrução return
    | SingleAssignment String AST                     -- Atribuição simples com identificador
    | SliceExpr AST AST AST                           -- Expressão de slicing (sub-array)
    | VarExpr String                                  -- Expressão variável
    | WhileStmt AST AST                               -- Instrução while
    deriving (Show)

-- Função de erro para análise sintática
parseError :: [Token] -> a
parseError tokens = error $ "Erro de análise sintática. Tokens restantes: " ++ show tokens

-- Função para gerar uma representação "bonita" da AST
prettyPrintAST :: AST -> String
prettyPrintAST ast = go 0 ast
  where
    indent n = replicate (n * 4) ' '
    
    go n (ArrayLiteral elems) =
      indent n ++ "└─ ArrayLiteral\n" ++ concatMap (go (n+1)) elems
    go n (AssignmentStmt asgn) =
      indent n ++ "└─ AssignmentStmt\n" ++ go (n+1) asgn
    go n (BlockStmt stmts) =
      indent n ++ "└─ BlockStmt\n" ++ concatMap (go (n+1)) stmts
    go n (BoolExpr b) =
      indent n ++ "└─ BoolExpr [BOOL: " ++ show b ++ "]\n"
    go n BreakStmt =
      indent n ++ "└─ BreakStmt\n"
    go n (CallExpr name args) =
      indent n ++ "└─ CallExpr\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (CallStmt name args) =
      indent n ++ "└─ CallStmt\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (CharExpr c) =
      indent n ++ "└─ CharExpr [CHAR: " ++ show c ++ "]\n"
    go n (DataStmt name body) =
      indent n ++ "└─ DataStmt [ID: " ++ name ++ "]\n" ++ go (n+1) body
    go n (FloatExpr f) =
      indent n ++ "└─ FloatExpr [FLOAT: " ++ show f ++ "]\n"
    go n (FieldAccessExpr expr field) =
      indent n ++ "└─ FieldAccessExpr\n" ++
      indent (n+1) ++ "├─ Expression:\n" ++ go (n+2) expr ++
      indent (n+1) ++ "└─ Field: [ID: " ++ field ++ "]\n"
    go n (FunStmt name params types body) =
      indent n ++ "└─ FunStmt\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "├─ Params: " ++ showParams params ++ "\n" ++
      indent (n+1) ++ "└─ ReturnTypes: " ++ show types ++ "\n" ++
      go (n+1) body
    go n (IfStmt cond thenStmt elseStmt) =
      indent n ++ "└─ IfStmt\n" ++
      indent (n+1) ++ "├─ Condition:\n" ++ go (n+2) cond ++
      indent (n+1) ++ "├─ Then:\n" ++ go (n+2) thenStmt ++
      indent (n+1) ++ "└─ Else:\n" ++ go (n+2) elseStmt
    go n (IndexExpr expr idx) =
      indent n ++ "└─ IndexExpr\n" ++
      indent (n+1) ++ "├─ Array:\n" ++ go (n+2) expr ++
      indent (n+1) ++ "└─ Index:\n" ++ go (n+2) idx
    go n (IntExpr i) =
      indent n ++ "└─ IntExpr [INT: " ++ show i ++ "]\n"
    go n (IterateStmt expr body) =
      indent n ++ "└─ IterateStmt\n" ++ go (n+1) expr ++ go (n+1) body
    go n (LValueAssignment lval expr) =
      indent n ++ "└─ LValueAssignment\n" ++
      indent (n+1) ++ "├─ LValue:\n" ++ go (n+2) lval ++
      indent (n+1) ++ "└─ Expr:\n" ++ go (n+2) expr
    go n (MainProgram a) =
      indent n ++ "└─ MainProgram\n" ++ go (n+1) a
    go n (MainWithDecls a decls) =
      indent n ++ "└─ MainWithDecls\n" ++ 
      indent (n+1) ++ "├─ Main:\n" ++ go (n+2) a ++
      indent (n+1) ++ "└─ Declarações:\n" ++ concatMap (go (n+2)) decls
    go n (MultipleAssignment n1 n2) =
      indent n ++ "└─ MultipleAssignment\n" ++
      indent (n+1) ++ "└─ Names: [ID: " ++ n1 ++ "], [ID: " ++ n2 ++ "]\n"
    go n (NewExpr typeName) =
      indent n ++ "└─ NewExpr [TYPEID: " ++ typeName ++ "]\n"
    go n (NewExprArgs typeName args) =
      indent n ++ "└─ NewExprArgs [TYPEID: " ++ typeName ++ "]\n" ++
      indent (n+1) ++ "└─ Args:\n" ++ concatMap (go (n+2)) args
    go n (NewStmt typeName params body) =
      indent n ++ "└─ NewStmt\n" ++
      indent (n+1) ++ "├─ Type: [TYPEID: " ++ typeName ++ "]\n" ++
      indent (n+1) ++ "└─ Params: " ++ show params ++ "\n" ++
      go (n+1) body
    go n NullExpr =
      indent n ++ "└─ NullExpr [NULL]\n"
    go n (OpExpr op left right) =
      indent n ++ "└─ OpExpr [OP: " ++ op ++ "]\n" ++
      indent (n+1) ++ "├─ Left:\n" ++ go (n+2) left ++
      indent (n+1) ++ "└─ Right:\n" ++ go (n+2) right
    go n (PrintStmt expr) =
      indent n ++ "└─ PrintStmt\n" ++ go (n+1) expr
    go n (ProgramStmtList stmts) =
      indent n ++ "└─ ProgramStmtList\n" ++ concatMap (go (n+1)) stmts
    go n (ReadStmt name) =
      indent n ++ "└─ ReadStmt [ID: " ++ name ++ "]\n"
    go n (RecordExpr fields) =
      indent n ++ "└─ RecordExpr\n" ++
      concatMap (\(name, expr) -> indent (n+1) ++ "├─ " ++ name ++ ": \n" ++ go (n+2) expr) fields
    go n (ReturnStmt exprs) =
      indent n ++ "└─ ReturnStmt\n" ++ concatMap (go (n+1)) exprs
    go n (SingleAssignment name expr) =
      indent n ++ "└─ SingleAssignment\n" ++
      indent (n+1) ++ "├─ Name: [ID: " ++ name ++ "]\n" ++
      indent (n+1) ++ "└─ Expr:\n" ++ go (n+2) expr
    go n (SliceExpr arr start end) =
      indent n ++ "└─ SliceExpr\n" ++
      indent (n+1) ++ "├─ Array:\n" ++ go (n+2) arr ++
      indent (n+1) ++ "├─ Start:\n" ++ go (n+2) start ++
      indent (n+1) ++ "└─ End:\n" ++ go (n+2) end
    go n (VarExpr s) =
      indent n ++ "└─ VarExpr [ID: " ++ s ++ "]\n"
    go n (WhileStmt cond stmt) =
      indent n ++ "└─ WhileStmt\n" ++
      indent (n+1) ++ "├─ Condition:\n" ++ go (n+2) cond ++
      indent (n+1) ++ "└─ Body:\n" ++ go (n+2) stmt

    showParams [] = "[]"
    showParams ps = concatMap (\(n,t) -> "[ID: " ++ n ++ " :: " ++ t ++ "] ") ps
}
