
module CompilerBackend where
import AbsLatte
import ErrM
import Data.String.Utils
import Data.List.Split
-- import Data.List.Index

-- change Success to Error later --
failure :: Show a => a -> Result
failure x = Success "" None

-- create function variable with type and name
createFunction :: String -> String -> Variable
createFunction t n = Variable t n n True 0 True False

-- create Variable with type, name and block level
createVariable :: String -> String -> Integer -> Variable
createVariable t n l = Variable t n n False l False True

-- create Variable type, name and block level that was declared
createDeclaredVariable :: String -> String -> Integer -> Variable
createDeclaredVariable t n l = Variable t n n False l False False

-- create Variable with type, name, alias and level
createDeclaredVariableWithAlias :: String -> String -> String -> Integer -> Variable
createDeclaredVariableWithAlias t n a l = Variable t n a False l False False

-- create Variable for label with name
createLabel :: String -> Variable
createLabel name = Variable "label" name name False 0 False True

-- create Variable with type, name and alias
createArgumentVariable :: String -> String -> String -> Variable
createArgumentVariable t n a = Variable t n a False 1 False True

-- create Variable for counting blocks
createBlockCounter :: Variable
createBlockCounter = BlockCounter "b" "e" "e" False (-1) False False 0 []

incrementBlockCount :: Variable -> Variable
incrementBlockCount var = BlockCounter "b" "e" "e" False (-1) False False (count var + 1) (prevBlocks var ++ [count var + 1])

decrementCurrentBlock :: Variable -> Variable
decrementCurrentBlock var = BlockCounter "b" "e" "e" False (-1) False False (count var) (init $ prevBlocks var)

getBlockCounter :: [Variable] -> Variable
getBlockCounter = head

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

checkDuplicate :: String -> [String] -> Bool
checkDuplicate x [] = False
checkDuplicate x (head: tail) = (x == head) || checkDuplicate x tail

areSameType :: Variable -> Variable -> Bool
areSameType v1 v2 = varType v1 == varType v2

getType :: [Variable] -> String -> String
getType v arg = case v of
  (x:xs) -> do
    let arr = filter (\x -> alias x == arg) v
    if null arr then "none" else  varType $ last arr
  [] -> "none"

getFuncType :: Variable -> [String]
getFuncType f = splitOn " " (varType f)

getFuncVariable :: [Variable] -> String -> Variable
getFuncVariable v s = last (filter (\x -> isFunc x && name x == s) v)

checkDuplicateExist :: [String] -> [String] -> Bool
checkDuplicateExist [] _ = False
checkDuplicateExist (x:xs) vals = checkDuplicate x vals || checkDuplicateExist xs vals

getIndex :: Eq a => a -> [a] -> Integer
getIndex i v = findIndex 0 i v

findIndex :: Eq a => Integer -> a -> [a] -> Integer
findIndex start character (x:xs) = if x == character then start else findIndex (start + 1) character xs
findIndex start character [] = 0

combineArrays :: [a] -> [b] -> [(a ,b)]
combineArrays [x] [y] = [(x,y)]
combineArrays (x:xs) (y:ys) = [(x, y)] ++ combineArrays xs ys
combineArrays [] [] = []

getAllocationLine :: String -> String -> String
getAllocationLine a b = a ++ " = alloca " ++ b

getStoreLine :: Variable -> Variable -> String
getStoreLine v1 v2 = "store " ++ varType v1 ++ " %" ++ name v1 ++ ", " ++ varType v2 ++ "* " ++ alias v2

nextVarName :: [Variable] -> String
nextVarName x = "%" ++ show (1 + length (filter (\z -> isGenerated z) x))

nextVarNum :: [Variable] -> String
nextVarNum v = show (1 + length (filter (\z -> isGenerated z) v))

-- copy arguments to vars --
createFunctionDefinition :: String -> String -> String -> String -> String
createFunctionDefinition retType name args body = "define " ++ retType ++ " @" ++ name ++ "(" ++ args ++ ") {" ++ body ++ "}"

transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x

compileProgram :: Program -> String
compileProgram p = case p of
  Program topdefs -> do
    let functions = (map transFunc topdefs) ++ getAvailableFunctions
    -- line (map (transTopDef functions) topdefs)
    concatArray $ map (\x -> line x ++ "\n\n") (map (transTopDef functions) topdefs)

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> Success string (ExpContext string string [])

getAvailableFunctions :: [Variable]
getAvailableFunctions = [createFunction "void i32" "printInt",
                          createFunction "void i8*" "printString",
                          createFunction "void" "error",
                          createFunction "i32 i32" "readInt",
                          createFunction "i8* i8*" "readString"]

transFunc :: TopDef -> Variable
transFunc x = case x of 
  FnDef type_ ident args block -> do
    let fType = transType type_
    let fName = line $ transIdent ident
    let fArgTypes = map (argType . context) (map transArg args)
    let fDesc = line fType ++ " " ++ concatWithSeparator fArgTypes " "
    createFunction fDesc fName


transTopDef :: [Variable] -> TopDef -> Result
transTopDef v x = case x of
  FnDef type_ ident args block -> do 
    let translatedArgs = map transArg args
    let agumentContexts = map context translatedArgs
    let argumentVars = combineArrays agumentContexts [1..length agumentContexts]
    let variables = map (\x -> createArgumentVariable (argType (fst x)) ((argName . fst) x) ("%" ++ show (snd x))) argumentVars

    let argumentsLine = concatArguments $ map line translatedArgs
    let allocArguments = map (\x -> getAllocationLine (alias x) (varType x)) variables
    let storeArguments = map (\x -> getStoreLine x x) variables

    let ftype = transType type_
    let fName = line $ transIdent ident

    let blockCounter = [createBlockCounter]
    
    let blockLine = line (transBlock block (blockCounter ++ v ++ variables) 0)
    let allocAndStoreArguments = "\n" ++ concatWithSeparator allocArguments "\n" ++ "\n" ++ concatWithSeparator storeArguments "\n"

    let result = createFunctionDefinition (line ftype) fName argumentsLine (allocAndStoreArguments ++ blockLine)
    Success result None

transArg :: Arg -> Result
transArg x = case x of
  Arg type_ ident -> do
    let t = transType type_
    let id = transIdent ident
    let context = ArgContext (line t) (line id)
    Success (line t ++ " %" ++ line id) (ArgContext (line t) (line id))
transBlock :: Block -> [Variable] -> Integer -> Result
transBlock x v l = case x of
  Block stmts -> do
    let newVars = (incrementBlockCount $ getBlockCounter v) : (tail v)

    let currentBlock = last $ prevBlocks (getBlockCounter newVars)
    let st = translateStatements stmts newVars currentBlock
    let statements = fst st
    let names = map (argName . context) statements
    let hasDuplicateNames = checkDuplicateExist names names
    -- if hasDuplicateNames then failure "Duplicate name found" None else
    let a = concatWithSeparator (map line statements) "\n"
    let retVars = (decrementCurrentBlock $ getBlockCounter (snd st)) : (tail (snd st))
    -- Success ("\n" ++ a ++ "\n" ++ (show . snd) st) (BlockContext retVars)
    Success ("\n" ++ a ++ "\n") (BlockContext (snd st))

-- use foldl
translateStatements :: [Stmt] -> [Variable] -> Integer -> ([Result], [Variable])
translateStatements (x:xs) v l = do
  let result = transStmt x v l
  let updatedVars = (vars . context) result
  let res = translateStatements xs updatedVars l
  let f = fst res
  (result : f, snd res)
translateStatements [] v l = ([], v)

-- use foldl
translateExpressions :: [Expr] -> [Variable] -> Integer -> ([Result], [Variable])
translateExpressions (x:xs) v l = do
  let result = transExpr x v l
  let updatedVars = (usedVars . context) result
  let res = translateExpressions xs updatedVars l
  let f = fst res
  (result : f, snd res)
translateExpressions [] v l = ([], v)

accumulate :: Result -> Result -> Result
accumulate r1 r2 = case context r1 of
  None -> Success (line r2) (StmtContext (usedVars (context r2)))
  _ -> Success (line r1 ++ "\n" ++ line r2) (StmtContext (usedVars (context r1) ++ usedVars (context r2)))

isBlockStatement :: Stmt -> Bool
isBlockStatement stmt = case stmt of
  BStmt _ -> True
  _ -> False

transStmt :: Stmt -> [Variable] -> Integer -> Result
transStmt x v l = case x of
  Empty -> Success "" (StmtContext v)
  BStmt block -> transBlock block v l
  Decl type_ items -> do
     let tp = line $ transType type_
     let emptyResult = Success "" None
     foldl (\a b -> accumulate a (transItem v l tp b)) emptyResult items
  Ass ident expr -> do
    let e = transExpr expr v l
    let e_ctx = context e
    let i = transIdent ident
    let i_ctx = context i
    let varName = expVar i_ctx
    let declaredVar = getDeclaredVariable varName v
    let vType = getType v (alias declaredVar)
    let result = line e ++ "\nstore " ++ expType e_ctx ++ " " ++ expVar e_ctx ++ ", " ++ vType ++ "* %" ++ alias declaredVar
    let newVars = usedVars e_ctx
    Success result (StmtContext newVars)
  Incr ident -> do 
    let statement = translateUnaryStatement ident v l
    Success (replace "_op_" "add" (line statement)) (context statement)
  Decr ident -> do
    let statement = translateUnaryStatement ident v l
    Success (replace "_op_" "sub" (line statement)) (context statement)
  Ret expr -> do 
    let expression = transExpr expr v l
    Success (line expression ++ "\nret " ++ (expType . context) expression ++ " " ++ (expVar . context) expression) (StmtContext v)
  VRet -> Success "ret void" (StmtContext v)
  Cond expr stmt -> do
    let expression = transExpr expr v l
    let newVars = (usedVars . context) expression
    let breakVar = nextVarName newVars
    let breakVarNum = nextVarNum newVars

    let level = if isBlockStatement stmt then l else last $ prevBlocks (getBlockCounter newVars)

    let statement = transStmt stmt (newVars ++ [createLabel breakVar]) level
    let retVars = vars (context statement)
    let breakVar2 = nextVarName retVars
    let breakVar2Num = nextVarNum retVars
    let expLine = (expVar . context) expression
    let break = "\nbr i1 " ++ expLine ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVarNum
    Success (line expression ++ break ++ line statement ++ "\n;<label> " ++ breakVar2Num) (StmtContext (retVars ++ [createLabel breakVar2]))
  CondElse expr stmt1 stmt2 -> do
    let expression = transExpr expr v l
    let newVars = (usedVars . context) expression
    let breakVar = nextVarName newVars
    let breakVarNum = nextVarNum newVars
    let statement1 = transStmt stmt1 (newVars ++ [createLabel breakVar]) l
    let stmt1Vars = vars (context statement1)

    let breakVar2 = nextVarName stmt1Vars
    let breakVar2Num = nextVarNum stmt1Vars
    let statement2 = transStmt stmt2 (stmt1Vars ++ [createLabel breakVar2]) l
    let retVars = vars (context statement2)
    let expLine = (expVar . context) expression
    let break = "\nbr i1 " ++ expLine ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVarNum
    let stmt1Line = line statement1
    let stmt2Line = line statement2
    let breakVar3 = nextVarName retVars
    let finalBreak = "br label " ++ breakVar3
    let finalBreakNum = nextVarNum retVars
    Success (line expression ++ break ++ stmt1Line ++ finalBreak ++ "\n;<label>:" ++ breakVar2Num ++ "\n" ++ stmt2Line ++ finalBreak ++ "\n;<label>:" ++ finalBreakNum) 
      (StmtContext (retVars ++ [createLabel breakVar3]))
  While expr stmt -> do
    let whileLabelVar = nextVarName v
    let whileLabel = "br label " ++ whileLabelVar ++ "\n;<label>:" ++ nextVarNum v ++ "\n"

    let expression = transExpr expr (v ++ [createLabel whileLabelVar]) l
    let expVars = usedVars (context expression)
    let stmtLabelVar = nextVarName expVars
    let stmtLabel = ";<label>:" ++ nextVarNum expVars ++ "\n"

    let level = if isBlockStatement stmt then l else last $ prevBlocks (getBlockCounter expVars)

    let statement = transStmt stmt (expVars ++ [createLabel stmtLabelVar]) level
    let stmtVars = vars (context statement)
    let continuationVar = nextVarName stmtVars
    let continuationLabel = ";<label>:" ++ nextVarNum stmtVars ++ "\n"
    let endStmtBreak = "br label " ++ whileLabelVar ++ "\n"
    let breakLine = "br i1 " ++ expVar (context expression) ++ ", label " ++ stmtLabelVar ++ ", label " ++ continuationVar ++ "\n"
    let returnLine = whileLabel ++ line expression ++ "\n" ++ breakLine ++ stmtLabel ++ line statement ++ endStmtBreak ++ continuationLabel
    Success returnLine (StmtContext (stmtVars ++ [createLabel continuationVar]))
  SExp expr -> do
    let expression = transExpr expr v l
    Success (line expression) (StmtContext (usedVars (context expression)))

nameVariable :: String -> [Variable] -> String
nameVariable s vars = case last $ prevBlocks (getBlockCounter vars) of
                        1 -> s
                        num -> s ++ show num



transItem :: [Variable] -> Integer -> String -> Item -> Result
transItem v l t x = case x of
  NoInit ident -> do 
    let id = line $ transIdent ident
    let newName = nameVariable id v
    Success ("%" ++ newName ++ " = alloca " ++ t) (ExpContext t id (v ++ [createDeclaredVariableWithAlias t id newName l]))
  Init ident expr -> do 
    let res = transExpr expr v l
    let id = line $ transIdent ident
    let exp = line res
    let ctx = context res
    let vars = usedVars ctx
    let newName = nameVariable id v
    -- type check
    let def = "%" ++ newName ++ " = alloca " ++ expType ctx
    let init = "store " ++ expType ctx ++ " " ++ expVar ctx ++ ", " ++ t ++"* %" ++ newName
    Success (line res ++ def ++ "\n" ++ init) (ExpContext (expType ctx) newName (vars ++ [createDeclaredVariableWithAlias t id newName l]))
transType :: Type -> Result
transType x = case x of
  Int -> Success "i32" None
  Str -> Success "i8*" None
  Bool -> Success "i1" None
  Void -> Success "void" None
  Fun type_ types -> Success "Fun type" None

getDeclaredVariable :: String -> [Variable] -> Variable
getDeclaredVariable n vars = do
  let filted = filter (\x -> name x == n) vars
  let prevDeclaredVars = concatMap (\x -> filter (\z -> level z == x) filted) (prevBlocks $ getBlockCounter vars)
  case prevDeclaredVars of
    [] -> EmptyVar
    x -> last x

transExpr :: Expr -> [Variable] -> Integer -> Result
transExpr x v level = case x of
  EVar ident -> do
    let id = transIdent ident
    let name = line id
    let declaredVar = getDeclaredVariable name v
    let tp = getType v (alias declaredVar)
    let var = nextVarName v
    Success (var ++ " = load " ++ tp ++ ", " ++ tp ++ "* %" ++ (alias declaredVar)) (ExpContext tp var (v ++ [createVariable tp var level]))
  ELitInt integer -> Success "" (ExpContext "i32" (show integer) v)
  ELitTrue -> Success "" (ExpContext "i1" "1" v)
  ELitFalse -> Success "" (ExpContext "i1" "0" v)
  EApp ident exprs -> do
    let resultsVariablesTuple = translateExpressions exprs v level
    let results = fst resultsVariablesTuple
    let variables = snd resultsVariablesTuple
    let mapedArguments = map (\x -> expType (context x) ++ " " ++ expVar (context x)) results
    let arguments = concatWithSeparator mapedArguments ","
    let lines = map line results
    let expLines = concatWithSeparator lines "\n"
    let id = transIdent ident
    let retVar = nextVarName variables
    let funcType = getFuncType (getFuncVariable variables (line id))
    let returnType = head funcType
    case returnType of
      "void" -> do
        let callLine = expLines ++ "\n" ++ "call " ++ returnType ++ " @" ++ line id ++ "(" ++ arguments ++ ")"
        Success callLine (ExpContext returnType retVar variables)
      _ -> do
        let callLine = expLines ++ "\n" ++ retVar ++ " = call " ++ returnType ++ " @" ++ line id ++ "(" ++ arguments ++ ")"
        let usedVars = variables ++ [createVariable returnType retVar level]
        Success callLine (ExpContext returnType retVar usedVars)
  EString string -> Success "" (ExpContext "i8*" ("\"" ++ string ++ "\"") v)
  Neg expr -> do 
    let expression = transExpr expr v level
    let expressionVar = expVar (context expression)
    let expVars = usedVars (context expression)
    let negVar = nextVarName expVars
    let negative = negVar ++ " = mul i32 -1, " ++ expressionVar
    Success (line expression ++ "\n" ++ negative) (ExpContext "i32" negVar (expVars ++ [createVariable "i32" negVar level]))
  Not expr -> failure x -- TODO: To be implemented --
  EMul expr1 mulop expr2 -> do
    let translated = translateBinaryExpression expr1 expr2 v level
    Success (replace "_op_" "mul" (line translated)) (context translated)
  EAdd expr1 addop expr2 -> do
    let translated = translateBinaryExpression expr1 expr2 v level
    Success (replace "_op_" "add" (line translated)) (context translated)
  ERel expr1 relop expr2 -> do
    let l = transExpr expr1 v level
    let lExpVar = expVar (context l)
    let o = transRelOp relop
    let newV = usedVars (context l)
    let r = transExpr expr2 newV level
    let vars = usedVars (context r)
    if expType (context l) == expType (context r) then do
      let var = nextVarName vars
      let newVar = createVariable (expType (context l)) var level
      let ctx = ExpContext (expType (context l)) var (vars ++ [newVar])
      let res = var ++ " = icmp " ++ line o ++ " " ++ expType (context l) ++ " " ++ lExpVar ++ ", " ++ expVar (context r)
      Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
    else failure "type mismatch"
  EAnd expr1 expr2 -> do
    let translated = translateBooleanExpression expr1 expr2 v level
    Success (replace "_op_" "and" (line translated)) (context translated)
  EOr expr1 expr2 -> do
    let translated = translateBooleanExpression expr1 expr2 v level
    Success (replace "_op_" "or" (line translated)) (context translated)


translateUnaryStatement :: Ident -> [Variable] -> Integer -> Result
translateUnaryStatement ident v l = do 
  let id = line $ transIdent ident
  let declaredVar = getDeclaredVariable id v
  let tp = getType v (alias declaredVar)
  let var = nextVarName v
  let newVars = v ++ [createVariable "i32" var l]
  let load = var ++ " = load i32, " ++ tp ++ "* %" ++ alias declaredVar ++ "\n"
  let incVar = nextVarName newVars
  let increment = incVar ++ " = _op_ i32 " ++ var ++ ", 1\n"
  let res = "store i32 " ++ incVar ++ ", i32* %" ++ alias declaredVar
  let retVars = newVars ++ [createVariable "i32" incVar l]
  Success (load ++ increment ++ res) (StmtContext retVars)


translateBinaryExpression :: Expr -> Expr -> [Variable] -> Integer -> Result
translateBinaryExpression expr1 expr2 v level = do
  let l = transExpr expr1 v level
  let lvars = usedVars (context l)
  let lvar = createVariable (expType $ context l) (expVar $ context l) level
  let r = transExpr expr2 lvars level
  let rvars = usedVars (context r)
  let rvar = createVariable (expType $ context r) (expVar $ context r) level

  if areSameType lvar rvar then do
    let opVarName = nextVarName rvars
    let res = opVarName ++ " = _op_ " ++ varType lvar ++ " " ++ alias lvar ++ ", " ++ alias rvar
    let retVars = rvars ++ [createVariable (varType lvar) opVarName level]
    let ctx = ExpContext (varType lvar) opVarName retVars
    Success (line l ++ "\n" ++ line r ++ "\n" ++ res ++ "\n") ctx
  else failure "type mismatch"

translateBooleanExpression :: Expr -> Expr -> [Variable] -> Integer -> Result
translateBooleanExpression expr1 expr2 v level = do
  let l = transExpr expr1 v level
  let lvars = usedVars (context l)
  let lvar = expVar (context l)
  let r = transExpr expr2 lvars level
  let rvars = usedVars (context r)
  let rvar = expVar (context r)
  let orVar = nextVarName rvars
  let res = orVar ++ " = _op_ i1 " ++ lvar ++ ", " ++ rvar
  let retVars = rvars ++ [createVariable "i1" orVar level]
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
