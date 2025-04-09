-- Aluno 1: Leandro Marcos Mendes Zanetti - 20.2.4182
-- Aluno 2: Pedro Parentoni de Almeida    - 20.1.4016

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Semantic
  ( analyze
  , typeCheck
  , typeCheckExpr
  , typeCheckStmt
  , typeCheckStmts
  , buildFunCtx
  , astToCode
  , Ty(..)
  , Error(..)
  , Check
  ) where

import Lexer (lexer)
import Parser (parser, AST(..))
import Data.Char (toLower)
import Data.List (intercalate, nub)
import System.IO (hPutStrLn, stderr)
import qualified Data.Map as Map
import Data.List (isSuffixOf)

-- Define o tipo do ambiente de tipos, mapeando nomes de tipos customizados para seus campos
type TypeEnv = Map.Map String (Map.Map String Ty)
globalTypeEnv :: TypeEnv
globalTypeEnv = Map.empty

-- Define tipos para variáveis, contexto e contexto de funções
type Var = String
type Ctx = [(Var, Ty)]
type FunCtx = [(String, ([Ty], Ty))]

-- Define os tipos que a linguagem suporta
data Ty = TInt 
        | TBool 
        | TCustom String 
        | TNull 
        | TArray Ty 
        deriving (Eq, Show)

-- Tipos para erros na verificação semântica
type Expected = Ty
type Found = Ty

-- Define os erros possíveis durante a checagem semântica
data Error
  = IncompatibleTypes Expected Found (Maybe String) (Maybe String)
  | UndefinedVariable Var (Maybe String) (Maybe String)
  | NotImplemented String (Maybe String) (Maybe String)

-- O tipo Check é usado para retornar ou uma lista de erros ou um resultado bem-sucedido
type Check a = Either [Error] a

-- Função que converte um tipo interno em sua representação textual
pType :: Ty -> String 
pType TInt         = "int"
pType TBool        = "bool"
pType (TCustom s)  = s
pType TNull        = "null"
pType (TArray t)   = "array of " ++ pType t

-- Função que adiciona destaque (cor vermelha) para mensagens de erro
highlight :: String -> String
highlight s = "\x1b[31m" ++ s ++ "\x1b[0m"

-- Instância Show para o tipo Error, que formata a mensagem de erro com informações de função e trecho de código
instance Show Error where
  show (IncompatibleTypes ex fd mFunc mSnippet) =
      "Error on function " ++ maybe "unknown" id mFunc ++ ":\n" ++
      "Incompatible types: expected " ++ pType ex ++ ", found " ++ pType fd ++ "\n" ++
      maybe "" (\sn -> highlight sn ++ "\n" ++ replicate (length sn) '^') mSnippet
  show (UndefinedVariable v mFunc mSnippet) =
      "Error on function " ++ maybe "unknown" id mFunc ++ ":\n" ++
      "Undefined variable: " ++ v ++ "\n" ++
      maybe "" (\sn -> highlight sn ++ "\n" ++ replicate (length sn) '^') mSnippet
  show (NotImplemented msg mFunc mSnippet) =
      "Error on function " ++ maybe "unknown" id mFunc ++ ":\n" ++
      msg ++ "\n" ++
      maybe "" (\sn -> highlight sn ++ "\n" ++ replicate (length sn) '^') mSnippet

-- Função que adiciona informações de localização (nome da função e trecho) a um erro
addLocation :: Error -> String -> String -> Error
addLocation (IncompatibleTypes e f _ _) func snippet = IncompatibleTypes e f (Just func) (Just snippet)
addLocation (UndefinedVariable v _ _) func snippet   = UndefinedVariable v (Just func) (Just snippet)
addLocation (NotImplemented msg _ _) func snippet    = NotImplemented msg (Just func) (Just snippet)

-- Retorna erro para variável indefinida
undefinedVar :: Var -> String -> Check a
undefinedVar v func = Left [UndefinedVariable v (Just func) (Just ("Undefined variable: " ++ v))]

-- Verifica se dois tipos são compatíveis, levando em conta tipos customizados e arrays
typeCompatible :: Ty -> Ty -> Bool
typeCompatible a b =
  a == b ||
  (isCustom a && b == TNull) ||
  (isCustom b && a == TNull) ||
  case (a, b) of
    (TArray t1, TArray t2) -> typeCompatible t1 t2
    _ -> False
  where
    isCustom (TCustom _) = True
    isCustom _ = False

-- Função auxiliar para verificar se um tipo é customizado
isCustom :: Ty -> Bool
isCustom (TCustom _) = True
isCustom _ = False

-- Cria mensagem de erro para tipos incompatíveis
incompatibleTypesMsg :: Maybe Var -> Expected -> Found -> String -> String -> Check a
incompatibleTypesMsg mVar e f func snippet =
  let baseMsg = case mVar of
                  Just v  -> "Type error for variable '" ++ v ++ "': expected " ++ pType e ++ ", found " ++ pType f
                  Nothing -> "Type error: expected " ++ pType e ++ ", found " ++ pType f
      fullMsg = baseMsg ++ "\n" ++ highlight snippet ++ "\n" ++ replicate (length snippet) '^'
  in Left [IncompatibleTypes e f (Just func) (Just fullMsg)]

-- Função auxiliar para acumular os resultados de duas checagens
accum2 :: Check a -> Check b -> (a -> b -> Check c) -> Check c
accum2 ca cb f =
  case (ca, cb) of
    (Right a, Right b) -> f a b
    (Left errs1, Left errs2) -> Left (errs1 ++ errs2)
    (Left errs, _) -> Left errs
    (_, Left errs) -> Left errs

-- Acumula os tipos dos argumentos em uma lista, aplicando a função f em cada AST
accumulateArgsSeq :: [AST] -> (AST -> TypeEnv -> Check (Ty, TypeEnv)) -> TypeEnv -> Check ([Ty], TypeEnv)
accumulateArgsSeq [] _ senv = Right ([], senv)
accumulateArgsSeq (a:as) f senv = do
  (ty, senv1) <- f a senv
  (tys, senv2) <- accumulateArgsSeq as f senv1
  return (ty : tys, senv2)

-- Extrai o nome de um tipo customizado, se aplicável
extractCustomName :: Ty -> Maybe String
extractCustomName (TCustom name) = Just name
extractCustomName _ = Nothing

-- Registra os tipos customizados presentes no contexto de variáveis no ambiente de tipos
registerCustomTypes :: Ctx -> TypeEnv -> TypeEnv
registerCustomTypes [] senv = senv
registerCustomTypes ((_, ty):xs) senv =
  case extractCustomName ty of
    Just name -> registerCustomTypes xs (Map.insertWith (\_ old -> old) name Map.empty senv)
    Nothing   -> registerCustomTypes xs senv

-- Constroi o contexto de funções (FunCtx) a partir da AST do programa
buildFunCtx :: AST -> FunCtx
buildFunCtx (ProgramStmtList stmts) = foldr addFun [] stmts
  where
    addFun (FunStmt name params ret _ ) ctx =
      let retType = case ret of
                      (t:_) -> convertType t
                      []    -> TNull
      in (name, (map (convertType . snd) params, retType)) : ctx
    addFun _ ctx = ctx
buildFunCtx _ = []

-- Contexto das funções built-in
builtInFunCtx :: FunCtx
builtInFunCtx = [("len", ([TArray TInt], TInt))]

-- Nomes das funções built-in
builtInNames :: [String]
builtInNames = map fst builtInFunCtx

-- Converte uma string em um tipo interno (Ty), tratando também arrays e tipos customizados
convertType :: String -> Ty
convertType s
  | "[]" `isSuffixOf` s = TArray (convertType (take (length s - 2) s))
  | map toLower s == "int"  = TInt
  | map toLower s == "bool" = TBool
  | otherwise               = TCustom s

-- Extrai o nome de uma função a partir de um nó AST de função
extractFunctionName :: AST -> String
extractFunctionName (FunStmt name _ _ _) = name
extractFunctionName _ = "unknown"

-- Busca o tipo de um campo em um tipo customizado, utilizando o ambiente de tipos
lookupField :: Ty -> String -> TypeEnv -> Maybe Ty
lookupField (TCustom name) field senv = Map.lookup name senv >>= Map.lookup field
lookupField _ _ _ = Nothing

-- Atualiza o campo de um tipo customizado no ambiente de tipos, se necessário
updateField :: Ty -> String -> Ty -> TypeEnv -> TypeEnv
updateField (TCustom name) field fType senv =
  let fields = Map.findWithDefault Map.empty name senv
      finalType = case fType of
                    TNull -> TCustom name
                    _     -> fType
      newFields = case Map.lookup field fields of
                    Just _ -> fields
                    Nothing      -> Map.insert field finalType fields
  in Map.insert name newFields senv
updateField _ _ _ senv = senv

-- Retorna um campo numérico padrão de um tipo customizado, se existir
defaultNumericField :: Ty -> TypeEnv -> Maybe (String, Ty)
defaultNumericField (TCustom name) senv = do
  fields <- Map.lookup name senv
  let numericFields = [ (f, t) | (f, t) <- Map.toList fields, t == TInt ]
  case numericFields of
    [pair] -> Just pair
    _      -> Nothing
defaultNumericField _ _ = Nothing

-- Encontra um trecho (snippet) de um comando de retorno (return) dentro de uma lista de comandos
findReturnSnippet :: [AST] -> String
findReturnSnippet [] = ""
findReturnSnippet (stmt:stmts) =
  case stmt of
    ReturnStmt _ -> astToCode stmt
    BlockStmt inner ->
      let snip = findReturnSnippet inner
      in if null snip then findReturnSnippet stmts else snip
    IfStmt _ thenBlock elseBlock ->
      let snipThen = findReturnSnippet [thenBlock]
          snipElse = findReturnSnippet [elseBlock]
      in if not (null snipThen)
            then snipThen
            else if not (null snipElse)
                    then snipElse
                    else findReturnSnippet stmts
    _ -> findReturnSnippet stmts

-- Coleta os nomes das funções chamadas em uma AST
collectCalls :: AST -> [String]
collectCalls ast =
  case ast of
    CallStmt name args  -> name : concatMap collectCalls args
    CallExpr name args  -> name : concatMap collectCalls args
    OpExpr _ e1 e2      -> collectCalls e1 ++ collectCalls e2
    LValueAssignment e expr -> collectCalls e ++ collectCalls expr
    AssignmentStmt stmt -> collectCalls stmt
    BlockStmt stmts     -> concatMap collectCalls stmts
    ProgramStmtList stmts -> concatMap collectCalls stmts
    FunStmt _ _ _ body  -> collectCalls body
    IfStmt cond thenBlock elseBlock -> collectCalls cond ++ collectCalls thenBlock ++ collectCalls elseBlock
    ReturnStmt exprs    -> concatMap collectCalls exprs
    PrintStmt expr      -> collectCalls expr
    IndexExpr arr idx   -> collectCalls arr ++ collectCalls idx
    _ -> []

-- Verifica e gera avisos para funções que foram definidas mas nunca utilizadas
checkUnusedFunctions :: AST -> FunCtx -> [String]
checkUnusedFunctions ast funCtx =
  let called = nub (collectCalls ast)
      defined = [ name | (name, _) <- funCtx, not (name `elem` builtInNames) ]
      unused = filter (\name -> name /= "main" && not (name `elem` called)) defined
  in map (\name -> "Warning: Function '" ++ name ++ "' is defined but never used.") unused

--------------------------------------------------------------------------------
-- FUNÇÕES DE CHECAGEM DE TIPOS EM EXPRESSÕES
--------------------------------------------------------------------------------

-- Checa o tipo de uma expressão e retorna o tipo e o ambiente de tipos atualizado
typeCheckExpr :: Ctx -> FunCtx -> TypeEnv -> String -> AST -> Check (Ty, TypeEnv)
-- Caso de expressão inteira
typeCheckExpr _ _ senv _ (IntExpr _) = Right (TInt, senv)
-- Caso de expressão booleana
typeCheckExpr _ _ senv _ (BoolExpr _) = Right (TBool, senv)
-- Caso de expressão float (tratada como int)
typeCheckExpr _ _ senv _ (FloatExpr _) = Right (TInt, senv)  
-- Caso de expressão caractere (tratada como int)
typeCheckExpr _ _ senv _ (CharExpr _) = Right (TInt, senv)   
-- Caso de variável: procura no contexto; se não encontrar, gera erro
typeCheckExpr ctx _ senv func (VarExpr v) =
  case lookup v ctx of
    Just t -> Right (t, senv)
    Nothing -> undefinedVar v func
-- Caso de operação binária para igualdade e desigualdade
typeCheckExpr ctx fctx senv func (OpExpr op e1 e2)
  | op == "==" || op == "!=" = do
      (t1, senv1) <- typeCheckExpr ctx fctx senv func e1
      (t2, senv2) <- typeCheckExpr ctx fctx senv1 func e2
      if typeCompatible t1 t2
         then Right (TBool, senv2)
         else incompatibleTypesMsg Nothing t1 t2 func (astToCode (OpExpr op e1 e2))
-- Caso de operações de comparação (< e >)
  | op == "<" || op == ">" = do
      (t1, senv1) <- typeCheckExpr ctx fctx senv func e1
      case e2 of
        FieldAccessExpr {} -> do
           (t2, senv2) <- typeCheckExpr ctx fctx senv1 func e2
           if t1 == TInt && t2 == TInt 
              then Right (TBool, senv2)
              else incompatibleTypesMsg Nothing TInt (if t1 /= TInt then t1 else t2) func (astToCode (OpExpr op e1 e2))
        _ -> do
           (t2, senv2) <- typeCheckExpr ctx fctx senv1 func e2
           let t1' = case t1 of
                        TCustom _ -> maybe t1 snd (defaultNumericField t1 senv2)
                        _         -> t1
           let t2' = case t2 of
                        TCustom _ -> maybe t2 snd (defaultNumericField t2 senv2)
                        _         -> t2
           if t1' == TInt && t2' == TInt 
              then Right (TBool, senv2)
              else incompatibleTypesMsg Nothing TInt (if t1' /= TInt then t1' else t2') func (astToCode (OpExpr op e1 e2))
-- Caso de operações de comparação (<= e >=)
  | op == "<=" || op == ">=" = do
      (t1, senv1) <- typeCheckExpr ctx fctx senv func e1
      (t2, senv2) <- typeCheckExpr ctx fctx senv1 func e2
      let t1' = case t1 of
                  TCustom _ -> maybe t1 snd (defaultNumericField t1 senv2)
                  _         -> t1
      let t2' = case t2 of
                  TCustom _ -> maybe t2 snd (defaultNumericField t2 senv2)
                  _         -> t2
      if t1' == TInt && t2' == TInt
         then Right (TBool, senv2)
         else incompatibleTypesMsg Nothing TInt (if t1' /= TInt then t1' else t2') func (astToCode (OpExpr op e1 e2))
-- Outras operações binárias: verifica se os operandos são do mesmo tipo
  | otherwise =
      accum2 (typeCheckExpr ctx fctx senv func e1) (typeCheckExpr ctx fctx senv func e2)
             (\(t1, senv1) (t2, senv2) ->
                if t1 == t2 
                   then Right (t1, senv2)
                   else incompatibleTypesMsg Nothing t1 t2 func (astToCode (OpExpr op e1 e2)))
-- Caso de impressão: checa o tipo da expressão a ser impressa
typeCheckExpr ctx fctx senv func (PrintStmt expr) = do
  (_, senv1) <- typeCheckExpr ctx fctx senv func expr
  Right (TInt, senv1)

-- Checa expressão de indexação para arrays
typeCheckExpr ctx fctx senv func (IndexExpr arrExpr idxExpr) = do
  case arrExpr of
    NewExpr typeName -> do
      (idxType, senv1) <- typeCheckExpr ctx fctx senv func idxExpr
      if idxType /= TInt
        then incompatibleTypesMsg Nothing TInt idxType func (astToCode (IndexExpr arrExpr idxExpr))
        else Right (TArray (convertType typeName), senv1)
    _ -> do
      (rawArrType, senv1) <- typeCheckExpr ctx fctx senv func arrExpr
      (idxType, senv2) <- typeCheckExpr ctx fctx senv1 func idxExpr
      if idxType /= TInt
        then incompatibleTypesMsg Nothing TInt idxType func (astToCode (IndexExpr arrExpr idxExpr))
        else case rawArrType of
               TArray t -> Right (t, senv2)
               _ -> Right (rawArrType, senv2)

-- Checa a expressão de slice em arrays
typeCheckExpr ctx fctx senv func (SliceExpr arrExpr startExpr endExpr) = do
  (arrType, senv1)   <- typeCheckExpr ctx fctx senv func arrExpr
  (startType, senv2) <- typeCheckExpr ctx fctx senv1 func startExpr
  (endType, senv3)   <- typeCheckExpr ctx fctx senv2 func endExpr
  if startType /= TInt || endType /= TInt
    then incompatibleTypesMsg Nothing TInt (if startType /= TInt then startType else endType) func (astToCode (SliceExpr arrExpr startExpr endExpr))
    else case arrType of
           TArray t -> Right (TArray t, senv3)
           _ -> Left [NotImplemented ("Slice operation on non-array type: " ++ astToCode (SliceExpr arrExpr startExpr endExpr))
                        (Just func) (Just (astToCode (SliceExpr arrExpr startExpr endExpr)))]
  
-- Checa chamadas de funções no formato de statement
typeCheckExpr ctx fctx senv func (CallStmt name args) =
  case lookup name fctx of
    Nothing -> undefinedVar name func
    Just (paramTypes, retType) ->
      if length args /= length paramTypes
         then Left [NotImplemented ("Function " ++ name ++ " called with an incorrect number of arguments")
                        (Just func) (Just (astToCode (CallStmt name args)))]
         else do
            (argTypes, senv1) <- accumulateArgsSeq args (\a env -> typeCheckExpr ctx fctx env func a) senv
            let mismatches = filter (\(a, p, _) -> not (typeCompatible p a))
                              (zip3 argTypes paramTypes args)
            if not (null mismatches)
              then let details = intercalate "; " (map (\(a, p, argAST) ->
                              case argAST of 
                                VarExpr v -> "argument '" ++ v ++ "' expected " ++ pType p ++ " but found " ++ pType a
                                _ -> "expected " ++ pType p ++ " but found " ++ pType a) mismatches)
                   in Left [NotImplemented ("Function " ++ name ++ " called with incompatible argument types: " ++ details)
                                  (Just func) (Just (astToCode (CallStmt name args)))]
              else Right (retType, senv1)

-- Checa chamadas de funções no formato de expressão (CallExpr)
typeCheckExpr ctx fctx senv func (CallExpr name args) =
  case lookup name fctx of
    Nothing -> undefinedVar name func
    Just (paramTypes, retType) ->
      if length args /= length paramTypes
         then Left [NotImplemented ("Function " ++ name ++ " called with an incorrect number of arguments")
                        (Just func) (Just (astToCode (CallExpr name args)))]
         else do
            (argTypes, senv1) <- accumulateArgsSeq args (\a env -> typeCheckExpr ctx fctx env func a) senv
            let mismatches = filter (\(a, p, _) -> not (typeCompatible p a))
                              (zip3 argTypes paramTypes args)
            if not (null mismatches)
              then let details = intercalate "; " (map (\(a, p, argAST) ->
                              case argAST of 
                                VarExpr v -> "argument '" ++ v ++ "' expected " ++ pType p ++ " but found " ++ pType a
                                _ -> "expected " ++ pType p ++ " but found " ++ pType a) mismatches)
                   in Left [NotImplemented ("Function " ++ name ++ " called with incompatible argument types: " ++ details)
                                  (Just func) (Just (astToCode (CallExpr name args)))]
              else Right (retType, senv1)

-- Checa o acesso a campos de registros (record types)
typeCheckExpr ctx fctx senv func (FieldAccessExpr recExpr field) = do
  (recType, senv1) <- typeCheckExpr ctx fctx senv func recExpr
  case lookupField recType field senv1 of
    Just t -> Right (t, senv1)
    Nothing -> Left [NotImplemented ("Field access for field " ++ field ++ " not implemented for type " ++ pType recType)
                                  (Just func) (Just (astToCode (FieldAccessExpr recExpr field)))]
-- Cria uma nova instância de um tipo (usado para "new" em tipos customizados)
typeCheckExpr _ fctx senv _ (NewExpr typeName) =
  let t = convertType typeName in
  let senv' = case t of
                TCustom name -> if Map.member name senv then senv else Map.insert name Map.empty senv
                _ -> senv
  in Right (t, senv')
-- Processamento de Record Literals: cria um novo tipo customizado para o literal de registro
typeCheckExpr ctx fctx senv func (RecordExpr fields) = do
  let processField (fieldName, expr) = do
         (fieldType, senv') <- typeCheckExpr ctx fctx senv func expr
         return (fieldName, fieldType)
  fieldsTypes <- mapM processField fields
  let newTypeName = func ++ "_record"
  let newType = TCustom newTypeName
  let newSenv = Map.insert newTypeName (Map.fromList fieldsTypes) senv
  return (newType, newSenv)
-- Processamento de Array Literals: checa todos os elementos e garante que são do mesmo tipo
typeCheckExpr ctx fctx senv func (ArrayLiteral elems) = do
  if null elems
    then Right (TArray TNull, senv)
    else do
      (firstType, senv1) <- typeCheckExpr ctx fctx senv func (head elems)
      (restTypes, senv2) <- accumulateArgsSeq (tail elems) (\a env -> typeCheckExpr ctx fctx env func a) senv1
      let allSame = all (== firstType) restTypes
      if allSame
         then Right (TArray firstType, senv2)
         else Left [NotImplemented "Array literal with mismatched element types" (Just func) Nothing]
-- Caso de expressão null
typeCheckExpr _ _ senv func NullExpr = Right (TNull, senv)
-- Caso não implementado para alguma expressão
typeCheckExpr _ _ senv func ast = Left [NotImplemented ("Semantic check not implemented for expression: " ++ astToCode ast)
                                      (Just func) (Just (astToCode ast))]

--------------------------------------------------------------------------------
-- FUNÇÕES DE CHECAGEM DE TIPOS EM COMANDOS/STATEMENTS
--------------------------------------------------------------------------------

-- Checa o tipo de um statement e retorna o tipo resultante, o contexto de variáveis e o ambiente de tipos atualizado
typeCheckStmt :: Ctx -> FunCtx -> TypeEnv -> String -> AST -> Check (Ty, Ctx, TypeEnv)
-- Caso de assignment que encapsula outro comando
typeCheckStmt ctx fctx senv func (AssignmentStmt stmt) =
  typeCheckStmt ctx fctx senv func stmt

-- Assignment para variável (LValueAssignment) com VarExpr: verifica se a variável já está no contexto e se o tipo é compatível
typeCheckStmt ctx fctx senv func (LValueAssignment (VarExpr v) expr) = do
  (et, senv1) <- typeCheckExpr ctx fctx senv func expr
  case lookup v ctx of
    Just vt ->
      if typeCompatible vt et
         then Right (et, ctx, senv1)
         else incompatibleTypesMsg (Just v) vt et func (astToCode (LValueAssignment (VarExpr v) expr))
    Nothing -> Right (et, (v, et) : ctx, senv1)

-- Assignment para acesso a campos de registro
typeCheckStmt ctx fctx senv func (LValueAssignment (FieldAccessExpr recExpr field) expr) = do
  (recType, senv1) <- typeCheckExpr ctx fctx senv func recExpr
  (et, senv2) <- typeCheckExpr ctx fctx senv1 func expr
  case recType of
    TCustom _ -> case lookupField recType field senv2 of
                   Just fieldType ->
                     if typeCompatible fieldType et
                        then Right (et, ctx, senv2)
                        else incompatibleTypesMsg Nothing fieldType et func (astToCode (LValueAssignment (FieldAccessExpr recExpr field) expr))
                   Nothing ->
                     let senv3 = updateField recType field et senv2
                     in Right (et, ctx, senv3)
    _ -> incompatibleTypesMsg Nothing recType TNull func (astToCode (LValueAssignment (FieldAccessExpr recExpr field) expr))

-- Assignment para arrays com indexação
typeCheckStmt ctx fctx senv func (LValueAssignment (IndexExpr arrExpr idxExpr) expr) = do
  (rawArrType, senv1) <- typeCheckExpr ctx fctx senv func arrExpr
  let arrType = case arrExpr of
                  NewExpr typeName -> TArray (convertType typeName)
                  _                -> rawArrType
  (idxType, senv2) <- typeCheckExpr ctx fctx senv1 func idxExpr
  if idxType /= TInt
     then incompatibleTypesMsg Nothing TInt idxType func (astToCode (IndexExpr arrExpr idxExpr))
     else do
       (exprType, senv3) <- typeCheckExpr ctx fctx senv2 func expr
       case arrType of
         TArray elemType ->
           if typeCompatible elemType exprType
             then Right (exprType, ctx, senv3)
             else incompatibleTypesMsg Nothing elemType exprType func (astToCode (LValueAssignment (IndexExpr arrExpr idxExpr) expr))
         _ -> Left [NotImplemented ("Assignment to non-array type using index expression: " ++ astToCode (IndexExpr arrExpr idxExpr))
                        (Just func) (Just (astToCode (LValueAssignment (IndexExpr arrExpr idxExpr) expr)))]
           
-- Single assignment: trata atribuição direta à variável
typeCheckStmt ctx fctx senv func (SingleAssignment v expr) = do
  (et, senv1) <- typeCheckExpr ctx fctx senv func expr
  case lookup v ctx of
    Just vt ->
      if typeCompatible vt et
         then Right (et, ctx, senv1)
         else incompatibleTypesMsg (Just v) vt et func (astToCode (SingleAssignment v expr))
    Nothing -> Right (et, (v, et) : ctx, senv1)

-- Checa um bloco de comandos, atualizando o ambiente
typeCheckStmt ctx fctx senv func (BlockStmt stmts) = do
  (mTy, ctx', senv1) <- typeCheckStmts ctx fctx senv func stmts
  let t = maybe TNull id mTy
  Right (t, ctx', senv1)

-- Checa o comando if: verifica a condição e checa os blocos then e else, garantindo compatibilidade dos tipos de retorno
typeCheckStmt ctx fctx senv func (IfStmt cond thenBlock elseBlock) = do
  (condType, senvCond) <- typeCheckExpr ctx fctx senv func cond
  if condType /= TBool
    then incompatibleTypesMsg Nothing TBool condType func (astToCode cond)
    else do
      (thenTy, _, senvThen) <- typeCheckStmt ctx fctx senvCond func thenBlock
      (elseTy, _, senvElse) <- case elseBlock of
                                BlockStmt [] -> Right (thenTy, ctx, senvCond)
                                _            -> typeCheckStmt ctx fctx senvCond func elseBlock
      if typeCompatible thenTy elseTy
         then Right (thenTy, ctx, mergeEnvs senvThen senvElse)
         else let snippet' = if not (null (findReturnSnippet [thenBlock]))
                                then findReturnSnippet [thenBlock]
                                else findReturnSnippet [elseBlock]
              in incompatibleTypesMsg Nothing thenTy elseTy func snippet'

-- Checa o comando while: a condição deve ser booleana e o corpo é checado normalmente
typeCheckStmt ctx fctx senv func (WhileStmt cond body) = do
  (condType, senvCond) <- typeCheckExpr ctx fctx senv func cond
  if condType /= TBool
    then incompatibleTypesMsg Nothing TBool condType func (astToCode cond)
    else do
      (_, _, senvBody) <- typeCheckStmt ctx fctx senvCond func body
      return (TNull, ctx, mergeEnvs senvCond senvBody)

-- Trata chamada de função como comando
typeCheckStmt ctx fctx senv func stmt@(CallStmt _ _) = do
  (t, senv1) <- typeCheckExpr ctx fctx senv func stmt
  Right (t, ctx, senv1)

-- Checa definição de função: cria um novo contexto para os parâmetros e checa o corpo da função
typeCheckStmt ctx fctx senv func (FunStmt name params ret body) =
  let newCtx = [(v, convertType t) | (v, t) <- params]
      senv' = registerCustomTypes newCtx senv
  in case ret of
       (t:_) -> let declaredRetType = convertType t in
                case typeCheck newCtx fctx senv' name body of
                  Right (tBody, senv1) ->
                    if typeCompatible declaredRetType tBody
                       then Right (declaredRetType, ctx, senv1)
                       else incompatibleTypesMsg Nothing declaredRetType tBody name (astToCode body)
                  Left errs -> Left errs
       [] -> fmap (\(t, senv1) -> (t, ctx, senv1)) (typeCheck newCtx fctx senv' name body)

-- Checa o comando return: deve conter pelo menos uma expressão
typeCheckStmt ctx fctx senv func (ReturnStmt exprs) =
  case exprs of
    [] -> Left [NotImplemented "ReturnStmt without expression" (Just func) Nothing]
    (expr:_) -> do
      (t, senv1) <- typeCheckExpr ctx fctx senv func expr
      Right (t, ctx, senv1)

-- Para outros casos de statement, utiliza a checagem de expressão
typeCheckStmt ctx fctx senv func stmt = do
  (t, senv1) <- typeCheckExpr ctx fctx senv func stmt
  Right (t, ctx, senv1)

-- Checa uma lista de statements, acumulando o contexto, ambiente e erros
typeCheckStmts :: Ctx -> FunCtx -> TypeEnv -> String -> [AST] -> Check (Maybe Ty, Ctx, TypeEnv)
typeCheckStmts ctx fctx senv func stmts =
  let (finalMaybe, finalCtx, senvFinal, errs) =
        foldl (\(mLast, curCtx, curSenv, accErrs) stmt ->
                 case checkStmt curCtx fctx curSenv func stmt of
                   (mType, newCtx, newSenv, stmtErrs) -> 
                     ( if mType == Nothing then mLast else mType
                     , newCtx
                     , newSenv
                     , accErrs ++ stmtErrs)
              ) (Nothing, ctx, senv, []) stmts
  in if null errs then Right (finalMaybe, finalCtx, senvFinal) else Left errs

-- Função auxiliar que checa um único statement e retorna o resultado junto com possíveis erros
checkStmt :: Ctx -> FunCtx -> TypeEnv -> String -> AST -> (Maybe Ty, Ctx, TypeEnv, [Error])
checkStmt ctx fctx senv func stmt =
  case typeCheckStmt ctx fctx senv func stmt of
    Right (t, newCtx, newSenv) -> (Just t, newCtx, newSenv, [])
    Left errs -> (Nothing, ctx, senv, errs)

-- Função que mescla dois ambientes de tipos, utilizada principalmente para unir os ambientes de ramos if/else
mergeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
mergeEnvs = Map.unionWith Map.union

--------------------------------------------------------------------------------
-- FUNÇÃO PRINCIPAL DE CHECAGEM SEMÂNTICA
--------------------------------------------------------------------------------

-- Função que realiza a checagem semântica do programa inteiro
typeCheck :: Ctx -> FunCtx -> TypeEnv -> String -> AST -> Check (Ty, TypeEnv)
typeCheck ctx fctx senv func (ProgramStmtList stmts) =
  case typeCheckStmts ctx fctx senv func stmts of
    Right (mTy, _, senv1) -> Right (maybe TNull id mTy, senv1)
    Left errs -> Left errs
-- Checa uma definição de função isoladamente
typeCheck _ fctx senv func (FunStmt name params ret body) =
  case ret of
    (t:_) -> let declaredRetType = convertType t in
             case typeCheck [ (v, convertType t) | (v,t) <- params ] fctx senv name body of
               Right (tBody, senv1) ->
                 if typeCompatible declaredRetType tBody
                    then Right (declaredRetType, senv1)
                    else incompatibleTypesMsg Nothing declaredRetType tBody name (astToCode body)
               Left errs -> Left errs
    [] -> typeCheck [] fctx senv name body
-- Checa um bloco de comandos
typeCheck ctx fctx senv func (BlockStmt stmts) =
    case typeCheckStmts ctx fctx senv func stmts of
      Right (mTy, _, senv1) -> Right (maybe TNull id mTy, senv1)
      Left errs -> Left errs
-- Para outros casos, utiliza a checagem de expressão
typeCheck ctx fctx senv func ast = typeCheckExpr ctx fctx senv func ast

--------------------------------------------------------------------------------
-- FUNÇÃO PARA CONVERTER A AST DE VOLTA PARA CÓDIGO (ÚTIL PARA DEBUG)
--------------------------------------------------------------------------------

astToCode :: AST -> String
astToCode (IntExpr n) = show n
astToCode (BoolExpr b) = if b then "true" else "false"
astToCode (FloatExpr f) = show f
astToCode (CharExpr c) = show c
astToCode (VarExpr v) = v
astToCode (OpExpr op e1 e2) = astToCode e1 ++ " " ++ op ++ " " ++ astToCode e2
astToCode (LValueAssignment lv expr) = astToCode lv ++ " = " ++ astToCode expr
astToCode (AssignmentStmt stmt) = astToCode stmt
astToCode (BlockStmt stmts) = unlines (map astToCode stmts)
astToCode (ProgramStmtList stmts) = unlines (map astToCode stmts)
astToCode (FunStmt name _ _ body) = "function " ++ name ++ " {\n" ++ astToCode body ++ "\n}"
astToCode (PrintStmt expr) = "print(" ++ astToCode expr ++ ")"
astToCode (IndexExpr arr idx) = astToCode arr ++ "[" ++ astToCode idx ++ "]"
astToCode (CallStmt name args) = name ++ "(" ++ intercalate ", " (map astToCode args) ++ ")"
astToCode (CallExpr name args) = name ++ "(" ++ intercalate ", " (map astToCode args) ++ ")"
astToCode (ReturnStmt exprs) = "return " ++ unwords (map astToCode exprs)
astToCode (SingleAssignment v expr) = v ++ " = " ++ astToCode expr
astToCode (NewExpr typeName) = "new " ++ typeName
astToCode (FieldAccessExpr expr field) = astToCode expr ++ "." ++ field
astToCode (SliceExpr arr start end) = astToCode arr ++ "[" ++ astToCode start ++ ":" ++ astToCode end ++ "]"
astToCode (RecordExpr fields) = "{" ++ intercalate ", " (map (\(f, expr) -> f ++ ": " ++ astToCode expr) fields) ++ "}"
astToCode (ArrayLiteral elems) = "[" ++ intercalate ", " (map astToCode elems) ++ "]"
astToCode NullExpr = "null"
astToCode ast = "<unknown_code>"

-- Extrai um trecho (snippet) de código a partir da AST
extractSnippet :: AST -> String
extractSnippet = astToCode

--------------------------------------------------------------------------------
-- FUNÇÃO DE ANÁLISE SEMÂNTICA (PONTO DE ENTRADA)
--------------------------------------------------------------------------------

-- A função analyze recebe uma string com o código fonte, gera a AST e executa a checagem semântica.
-- Além disso, emite avisos para funções não utilizadas e retorna o resultado da checagem (tipo e ambiente de tipos) ou erros.
analyze :: String -> IO (Check (Ty, TypeEnv))
analyze input =
  let ast    = parser (lexer input)
      funCtx = builtInFunCtx ++ buildFunCtx ast  
      result = typeCheck [] funCtx globalTypeEnv "unknown" ast
  in do
      case ast of
        ProgramStmtList stmts ->
          mapM_ (hPutStrLn stderr . ("Warning: " ++) . id)
                (checkUnusedFunctions ast funCtx)
        _ -> return ()
      return result