
module CompilerBackend where
import AbsLatte
import ErrM

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
  Program topdefs -> concatArray $ map (\x -> line x) (map transTopDef topdefs)

transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> Success string (ExpContext string string)

transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef type_ ident args block -> do 
    let arguments = concatArguments $ map (\x -> line x) (map transArg args)
    let identity = transIdent ident
    let ftype = transType type_
    let blockContext = BlockContext [arguments]
    let b = transBlock block
    let result = "@define " ++ line ftype ++ " @" ++ line identity ++ "(" ++ arguments ++ ") " ++ line b
    Success result None

transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> do
    let t = transType type_
    let id = transIdent ident
    Success (line t ++ " %" ++ line id) None
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> do
    let a = concatWithSeparator (map (\x -> line x) (map transStmt stmts)) "\n"
    Success ("{\n" ++ a ++ "}\n") None
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> Success "" None
  BStmt block -> transBlock block
  Decl type_ items -> do
     let tp = transType type_
     let ids = map (\x -> (line x) ++ (line tp)) (map transItem items)
     let result = concatArguments ids
     Success result None
  Ass ident expr -> do
    let e = transExpr expr
    let e_ctx = context e
    let i = transIdent ident
    let i_ctx = context i
    let result = "store " ++ expType e_ctx ++ " " ++ expVar e_ctx ++ ", %" ++ line i
    Success result None
  Incr ident -> failure x
  Decr ident -> failure x
  Ret expr -> Success ("ret " ++ line (transExpr expr)) None
  VRet -> Success "ret void" None
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  SExp expr -> Success (line $ transExpr expr) None
transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> Success ("%" ++ line (transIdent ident) ++ " = alloca ") None
  Init ident expr -> do 
    let res = transExpr expr
    let id = transIdent ident
    let exp = line res
    let ctx = context res
    let def = line id ++ " = alloca "
    let init = "store " ++ expType ctx ++ ", " ++ expVar ctx ++ " " ++ line id

    Success (exp ++ line id ++ " = alloca \n" ++ init) None
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
