
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
concatWithSeparator [x] s = x
concatWithSeparator (x:xs) s = x ++ s ++ concatWithSeparator xs s
concatWithSeparator [] s = []

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

getType :: [Variable] -> String -> String
getType v arg = case v of
  (x:xs) -> do
    let arr = filter (\x -> name x == arg) v
    if null arr then "no_type" else  varType $ last arr
  [] -> "no_type"

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
  Ident string -> Success string (ExpContext string string [])

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
    -- Success ("\n{\n" ++ a ++ "\n}\n" ++ (show . snd) st) (BlockContext (snd st))
    Success ("\n" ++ a ++ "\n") (BlockContext (snd st))

translateStatements :: [Stmt] -> [Variable] -> ([Result], [Variable])
translateStatements (x:xs) v = do
  let result = transStmt x v
  let updatedVars = (vars . context) result
  let res = translateStatements xs updatedVars
  let f = fst res
  (result : f, snd res)
translateStatements [] v = ([], v)

translateExpressions :: [Expr] -> [Variable] -> ([Result], [Variable])
translateExpressions (x:xs) v = do
  let result = transExpr x v
  let updatedVars = (usedVars . context) result
  let res = translateExpressions xs updatedVars
  let f = fst res
  (result : f, snd res)
translateExpressions [] v = ([], v)

transStmt :: Stmt -> [Variable] -> Result
transStmt x v = case x of
  Empty -> Success "" (StmtContext v)
  BStmt block -> transBlock block v
  Decl type_ items -> do
     let tp = line $ transType type_
     let translatedItems = map transItem items
     let ids = map (\x -> replace "_tp_" tp (line x)) translatedItems
     let result = concatArguments ids
     let newVars = map (\x -> Variable tp (expVar (context x))) translatedItems
     Success result (StmtContext (v ++ newVars))
  Ass ident expr -> do
    let e = transExpr expr v
    let e_ctx = context e
    let i = transIdent ident
    let i_ctx = context i
    let varName = expVar i_ctx
    let filtered = map varType (filter (\x -> name x == varName) v)
    let result = line e ++ "\nstore " ++ expType e_ctx ++ " " ++ expVar e_ctx ++ ", " ++ last filtered ++ "* %" ++ line i
    let newVars = usedVars e_ctx
    Success result (StmtContext newVars)
  Incr ident -> do 
    let id = transIdent ident
    let tp = getType v (line id)
    let var = "%" ++ (show . length) v
    let newVars = v ++ [Variable "i32" var]
    let load = var ++ " = load i32* " ++ line id ++ "\n"
    let incVar = "%" ++ (show . length) newVars
    let increment = incVar ++ " = add i32 " ++ var ++ ", 1\n"
    let res = "store i32 " ++ incVar ++ ", i32* %" ++ line id
    let retVars = newVars ++ [Variable "i32" incVar]
    Success (load ++ increment ++ res) (StmtContext retVars)
  Decr ident -> do
    let id = transIdent ident
    let tp = getType v (line id)
    let var = "%" ++ (show . length) v
    let newVars = v ++ [Variable "i32" var]
    let load = var ++ " = load i32* " ++ line id ++ "\n"
    let incVar = "%" ++ (show . length) newVars
    let increment = incVar ++ " = sub i32 " ++ var ++ ", 1\n"
    let res = "store i32 " ++ incVar ++ ", i32* %" ++ line id
    let retVars = newVars ++ [Variable "i32" incVar]
    Success (load ++ increment ++ res) (StmtContext retVars)
  Ret expr -> do 
    let expression = transExpr expr v
    Success (line expression ++ "\nret " ++ (expType . context) expression ++ " " ++ (expVar . context) expression) (StmtContext v)
  VRet -> Success "ret void" (StmtContext v)
  Cond expr stmt -> do
    let expression = transExpr expr v
    let newVars = (usedVars . context) expression
    let breakVar = "%" ++ show (length newVars)
    let statement = transStmt stmt (newVars ++ [Variable "label" breakVar])
    let retVars = vars (context statement)
    let breakVar2 = "%" ++ show (length retVars)
    let break = "\nbr i1 " ++ ((expVar . context) expression) ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVar
    Success (line expression ++ break ++ line statement ++ "\n;<label> " ++ breakVar2) (StmtContext (retVars ++ [Variable "label" breakVar2]))
  CondElse expr stmt1 stmt2 -> do
    let expression = transExpr expr v
    let newVars = (usedVars . context) expression
    let breakVar = "%" ++ show (length newVars)
    let statement1 = transStmt stmt1 (newVars ++ [Variable "label" breakVar])
    let stmt1Vars = vars (context statement1)
    let statement2 = transStmt stmt2 stmt1Vars
    let retVars = vars (context statement2)
    let breakVar2 = "%" ++ show (length stmt1Vars)
    let break = "\nbr i1 " ++ ((expVar . context) expression) ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVar
    let stmt1Line = line statement1
    let stmt2Line = line statement2
    let finalBreak = "br label " ++ show (length retVars)
    Success (line expression ++ break ++ stmt1Line ++ finalBreak ++ "\n;<label> " ++ breakVar2 ++ "\n" ++ stmt2Line ++ finalBreak ++ "\n;<label>:" ++ show (length retVars)) 
      (StmtContext (retVars ++ [(Variable "label" breakVar2), (Variable "label" (show (length retVars)))]))
  While expr stmt -> do
    let expression = transExpr expr v
    let expVars = usedVars (context expression)
    let statement = transStmt stmt expVars
    let stmtVars = vars (context statement)

    let whileLabelVar = "%" ++ show (length v)
    let whileLabel = ";<label>:" ++ show (length v) ++ "\n"
    let stmtLabelVar = "%" ++ show (length expVars)
    let stmtLabel = ";<label>:" ++ show (length expVars) ++ "\n"
    let continuationVar = "%" ++ show (length stmtVars)
    let continuationLabel = ";<label>:" ++ show (length stmtVars) ++ "\n"
    let endStmtBreak = "br label " ++ whileLabelVar ++ "\n"
    let breakLine = "br i1 " ++ expVar (context expression) ++ ", label " ++ stmtLabelVar ++ ", label " ++ continuationVar ++ "\n"
    let returnLine = whileLabel ++ line expression ++ "\n" ++ breakLine ++ stmtLabel ++ line statement ++ endStmtBreak ++ continuationLabel
    Success returnLine (StmtContext stmtVars)
  SExp expr -> do
    let expression = transExpr expr v
    Success (line expression) (StmtContext (usedVars (context expression)))

transItem :: Item -> Result
transItem x = case x of
  NoInit ident -> do 
    let id = transIdent ident
    Success ("%" ++ line id ++ " = alloca _tp_") (context id)
  Init ident expr -> do 
    let res = transExpr expr []
    let id = transIdent ident
    let exp = line res
    let ctx = context res
    let def = "%" ++ line id ++ " = alloca " ++ expType ctx
    let init = "store " ++ expType ctx ++ " " ++ expVar ctx ++ ", _tp_* %" ++ line id
    Success (def ++ "\n" ++ init) (ExpContext (expType ctx)  (line id) [])
transType :: Type -> Result
transType x = case x of
  Int -> Success "i32" None
  Str -> Success "i8*" None
  Bool -> Success "i1" None
  Void -> Success "void" None
  Fun type_ types -> Success "Fun type" None
transExpr :: Expr -> [Variable] -> Result
transExpr x v = case x of
  EVar ident -> do
    let id = transIdent ident
    let name = line id
    let tp = getType v name
    let var = "%" ++ show (length v)
    let vars = v ++ [Variable tp name]
    Success (var ++ " = load " ++ tp ++ "* %" ++ line id) (ExpContext tp var vars)
  ELitInt integer -> Success "" (ExpContext "i32" (show integer) v)
  ELitTrue -> Success "1" (ExpContext "i1" "1" v)
  ELitFalse -> Success "0" (ExpContext "i1" "0" v)
  EApp ident exprs -> do
    let resultsVariablesTuple = translateExpressions exprs v
    let results = fst resultsVariablesTuple
    let variables = snd resultsVariablesTuple
    let mapedArguments = map (\x -> expType (context x) ++ " " ++ expVar (context x)) results
    let arguments = concatWithSeparator mapedArguments ","
    let lines = map line results
    let expLines = concatWithSeparator lines "\n"
    let id = transIdent ident
    -- let expressionVar = last variables
    let retVarNum = length variables
    let retVar = "%" ++ show retVarNum
    let funcType = getType variables (line id)
    let callLine = expLines ++ retVar ++ " = call " ++ funcType ++ " @" ++ (line id) ++ "(" ++ arguments ++ ")"
    let usedVars = variables ++ [Variable funcType retVar]
    Success callLine (ExpContext funcType retVar usedVars)
  EString string -> Success string (ExpContext "i8*" string v)
  Neg expr -> do 
    let expression = transExpr expr v
    let expressionVar = expVar (context expression)
    let expVars = usedVars (context expression)
    let varNum = show (length expVars)
    let negVar = "%" ++ varNum
    let negative = negVar ++ " = mul i32 -1, " ++ expressionVar
    Success (line expression ++ negative) (ExpContext "i32" negVar (expVars ++ [Variable "i32" negVar]))
  Not expr -> failure x -- To be implemented --
  EMul expr1 mulop expr2 -> do
    let l = transExpr expr1 v
    let lExpVar = expVar (context l)
    let o = transMulOp mulop
    let newV = usedVars (context l)
    let r = transExpr expr2 newV
    let vars = usedVars (context r)
    if expType (context l) == expType (context r) then do
      let var = "%" ++ show (length vars)
      let newVar = Variable (expType (context l)) var
      let ctx = ExpContext (expType (context l)) var (vars ++ [newVar])
      let res = var ++ " = " ++ line o ++ " " ++ expType (context l) ++ " " ++ expVar (context l) ++ ", " ++ expVar (context r)
      Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
    else failure "type mismatch"
  EAdd expr1 addop expr2 -> do
    let l = transExpr expr1 v
    let lExpVar = expVar (context l)
    let o = transAddOp addop
    let newV = usedVars (context l)
    let r = transExpr expr2 newV
    let vars = usedVars (context r)
    if expType (context l) == expType (context r) then do
      let var = "%" ++ show (length vars)
      let newVar = Variable (expType (context l)) var
      let ctx = ExpContext (expType (context l)) var (vars ++ [newVar])
      let res = var ++ " = " ++ line o ++ " " ++ expType (context l) ++ " " ++ expVar (context l) ++ ", " ++ expVar (context r)
      Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
    else failure "type mismatch"
  ERel expr1 relop expr2 -> do
    let l = transExpr expr1 v
    let lExpVar = expVar (context l)
    let o = transRelOp relop
    let newV = usedVars (context l)
    let r = transExpr expr2 newV
    let vars = usedVars (context r)
    if expType (context l) == expType (context r) then do
      let var = "%" ++ show (length vars)
      let newVar = Variable (expType (context l)) var
      let ctx = ExpContext (expType (context l)) var (vars ++ [newVar])
      let res = var ++ " = icmp " ++ line o ++ " " ++ expType (context l) ++ " " ++ expVar (context l) ++ ", " ++ expVar (context r)
      Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
    else failure "type mismatch"
  EAnd expr1 expr2 -> do
    let l = transExpr expr1 v
    let lvars = usedVars (context l)
    let lvar = expVar (context l)
    let r = transExpr expr2 lvars
    let rvars = usedVars (context l)
    let rvar = expVar (context r)
    let varNum = show (length rvars)
    let andVar = "%" ++ varNum
    let res = andVar ++ " = and i1 " ++ lvar ++ ", " ++ rvar
    let retVars = rvars ++ [Variable "i1" andVar]
    let ctx = ExpContext "i1" andVar retVars
    Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
  EOr expr1 expr2 -> do
    let l = transExpr expr1 v
    let lvars = usedVars (context l)
    let lvar = expVar (context l)
    let r = transExpr expr2 lvars
    let rvars = usedVars (context l)
    let rvar = expVar (context r)
    let varNum = show (length rvars)
    let orVar = "%" ++ varNum
    let res = orVar ++ " = or i1 " ++ lvar ++ ", " ++ rvar
    let retVars = rvars ++ [Variable "i1" orVar]
    let ctx = ExpContext "i1" orVar retVars
    Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
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
  LTH -> Success "slt" None
  LE -> Success "sle" None
  GTH -> Success "sgt" None
  GE -> Success "sge" None
  EQU -> Success "eq" None
  NE -> Success "ne" None
