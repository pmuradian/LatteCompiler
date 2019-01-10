
module CompilerBackend where
import AbsLatte
import ErrM
import Data.String.Utils

-- change Success to Error later --
failure :: Show a => a -> Result
failure x = Success "" None

concatArray :: [String] -> String
concatArray (x:xs) = x ++ concatArray xs
concatArray [] = "\n"

concatWithSeparator :: [String] -> String -> String
concatWithSeparator (x:xs) s = x ++ s ++ concatWithSeparator xs s
concatWithSeparator [] s = ""

concatArguments :: [String] -> String
concatArguments [x] = x
concatArguments (x:xs) = x ++ ", " ++ concatArguments xs
concatArguments [] = ""

compileProgram :: Program -> String
compileProgram p = case p of
  Program topdefs -> concatArray $ map line (map transTopDef topdefs)

checkDuplicate :: String -> [String] -> Bool
checkDuplicate x [] = False
checkDuplicate x (head: tail) = (x == head) || checkDuplicate x tail

-- getType :: [Variable] -> String -> String
-- getType v arg = 

checkDuplicateExist :: [String] -> [String] -> Bool
checkDuplicateExist [] _ = False
checkDuplicateExist (x:xs) vals = checkDuplicate x vals || checkDuplicateExist xs vals

createFunctionDefinition :: String -> String -> String -> String -> String
createFunctionDefinition retType name args body = "@define " ++ retType ++ " @" ++ name ++ "(" ++ args ++ ") " ++ body

transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> Success string (ExpContext string string)

transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type_ ident args block -> do 
    let translatedArgs = map transArg args
    -- get argument names
    let res = map (argName . context) translatedArgs
    -- check for duplicate argument names
    let checked = checkDuplicateExist res res
    -- if checked == True then failure "Arguments have duplicate names"

    let argCout = length res
    -- concatenate arguments for LLVM form
    let argumentsLine = concatArguments $ map line translatedArgs

    let identity = transIdent ident
    let ftype = transType type_

    let usedVariables = map (\x -> Variable ((argType . context) x) ((argName . context) x)) translatedArgs
    let blockContext = BlockContext usedVariables
    let b = transBlock block usedVariables
    -- get function declaration in LLVM
    let result = createFunctionDefinition (line ftype) (line identity) argumentsLine (line b)
    Success result None

transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> do
    let t = transType type_
    let id = transIdent ident
    let context = ArgContext (line t) (line id)
    Success (line t ++ " %" ++ line id) (ArgContext (line t) (line id))
transBlock :: Block -> [Variable] -> Result
transBlock x v = case x of
  Block stmts -> do
    let arbabik = show v
    let st = translateStatements stmts v
    let statements = fst st
    let names = map (argName . context) statements
    let hasDuplicateNames = checkDuplicateExist names names
    -- if hasDuplicateNames then failure "Duplicate name found" None else
    let a = concatWithSeparator (map line statements) "\n"
    Success ("{\n" ++ a ++ "}\n" ++ (show . snd) st) None

translateStatements :: [Stmt] -> [Variable] -> ([Result], [Variable])
translateStatements (x:xs) v = do
  let result = transStmt x v
  let updatedVars = (vars . context) result
  let res = translateStatements xs updatedVars
  let f = fst res
  let s = snd res
  ([result] ++ f, s ++ updatedVars)
translateStatements [] v = ([Success "" (StmtContext [])], [])
transStmt :: Stmt -> [Variable] -> Result
transStmt x v = case x of
  Empty -> Success "empty" None
  BStmt block -> transBlock block []
  Decl type_ items -> do
     let tp = line $ transType type_
     let translatedItems = map transItem items
     let ids = map (\x -> replace "_tp_" tp (line x)) translatedItems
     let result = concatArguments ids
     let newVars = map (\x -> Variable tp (expVar (context x))) translatedItems
     Success result (StmtContext (v ++ newVars))
  Ass ident expr -> do
    let e = transExpr expr
    let e_ctx = context e
    let i = transIdent ident
    let i_ctx = context i
    let varName = expVar i_ctx
    let filtered = map varType (filter (\x -> name x == varName) v)
    let result = "store " ++ expType e_ctx ++ " " ++ expVar e_ctx ++ ", " ++ last filtered ++ " %" ++ line i
    Success result (StmtContext [])
  Incr ident -> Success "i++\n" None
  Decr ident -> Success "i--\n" None
  Ret expr -> do 
    let expression = transExpr expr
    Success ("ret " ++ line expression) (StmtContext [])
  VRet -> Success "ret void" None
  Cond expr stmt -> Success "if something {}\n" None
  CondElse expr stmt1 stmt2 -> Success "else {}\n" None
  While expr stmt -> Success "while something {}\n" None
  SExp expr -> Success (line $ transExpr expr) None

transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> do 
    let id = transIdent ident
    Success ("%" ++ line id ++ " = alloca _tp_") (context id)
  Init ident expr -> do 
    let res = transExpr expr
    let id = transIdent ident
    let exp = line res
    let ctx = context res
    let def = "%" ++ line id ++ " = alloca " ++ expType ctx
    let init = "store " ++ expType ctx ++ " " ++ expVar ctx ++ ", _tp_ %" ++ line id
    Success (def ++ "\n" ++ init) (ExpContext (expType ctx)  (expVar ctx))
transType :: Type -> Result
transType x = case x of
  Int -> Success "i32" None
  Str -> Success "i8*" None
  Bool -> Success "i1" None
  Void -> Success "void" None
  Fun type_ types -> Success "Fun type" None
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> transIdent ident
  ELitInt integer -> Success (show integer) (ExpContext "i32" (show integer))
  ELitTrue -> Success "1" None
  ELitFalse -> Success "0" None
  EApp ident exprs -> failure x
  EString string -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> do
    let l = transExpr expr1
    let o = transMulOp mulop
    let r = transExpr expr2
    let ctx = ExpContext (expType (context l)) "%0"
    if expType (context l) == expType (context r) then 
      Success (line r ++ line o ++ line l) ctx
    else failure "type mismatch"
  EAdd expr1 addop expr2 -> do
    let l = transExpr expr1
    let o = transAddOp addop
    let r = transExpr expr2
    let ctx = ExpContext (expType (context l)) "%1"
    if expType (context l) == expType (context r) then 
      Success (line r ++ line o ++ line l) ctx
    else failure "type mismatch"
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> Success "add" None
  Minus -> Success "sub" None
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> Success "mul" None
  Div -> Success "div" None
  Mod -> Success "mod" None
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x