
module CompilerBackend where
import AbsLatte
import ErrM
import Data.String.Utils
import Data.List.Split

-- change Success to Error later --
failure :: Show a => a -> Result
failure x = Success "" None

-- create function variable with type and name
createFunction :: String -> String -> Variable
createFunction t n = FuncVariable t n n True 0 True False False

markFunctionAsCurrent :: Variable -> Variable
markFunctionAsCurrent v = FuncVariable (varType v) (name v) (alias v) (isFunc v) (level v) True False True

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

findAndIncrementBlockCounter :: [Variable] -> [Variable]
findAndIncrementBlockCounter vars = (incrementBlockCount (getBlockCounter vars)) : (tail vars)

decrementCurrentBlock :: Variable -> Variable
decrementCurrentBlock var = BlockCounter "b" "e" "e" False (-1) False False (count var) (init $ prevBlocks var)

findAndDecrementBlockCounter :: [Variable] -> [Variable]
findAndDecrementBlockCounter vars = (decrementCurrentBlock (getBlockCounter vars)) : (tail vars)

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

checkDuplicate :: Variable -> [Variable] -> Variable
checkDuplicate x [] = EmptyVar
checkDuplicate x (head: tail) = if (name x == name head) && (level x == level head) then x else checkDuplicate x tail

areSameType :: Variable -> Variable -> Bool
areSameType v1 v2 = varType v1 == varType v2

getType :: [Variable] -> String -> String
getType v arg = case v of
  (x:xs) -> do
    let arr = filter (\x -> alias x == arg) v
    if null arr then "none" else  varType $ last arr
  [] -> "none"

getFuncType :: Variable -> [String]
getFuncType f = case splitOn " " (varType f) of
  [something ,""] -> [something]
  other -> other

getFuncVariable :: [Variable] -> String -> Variable
getFuncVariable v s = last (filter (\x -> isFunc x && name x == s) v)

checkForDuplicate :: [Variable] -> [Variable] -> Variable
checkForDuplicate [] _ = EmptyVar
checkForDuplicate (x:xs) vals = if checkDuplicate x vals /= EmptyVar then x else checkForDuplicate xs vals

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
getStoreLine v1 v2 = "store " ++ varType v1 ++ " %" ++ name v1 ++ ", " ++ varType v2 ++ "* %" ++ alias v2

nextVarName :: [Variable] -> String
nextVarName x = "%" ++ show (1 + length (filter (\z -> isGenerated z) x))

nextVarNum :: [Variable] -> String
nextVarNum v = show (1 + length (filter (\z -> isGenerated z) v))

-- copy arguments to vars --
createFunctionDefinition :: String -> String -> String -> String -> String -> String
createFunctionDefinition retType name args body retString = "define " ++ retType ++ " @" ++ name ++ "(" ++ args ++ ") {" ++ body ++ "\n" ++ retString ++ "\n}"

transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x

getContextVars :: Result -> [Variable]
getContextVars r = case context r of
  None -> []
  StmtContext {} -> vars $ context r
  ExpContext {} -> usedVars $ context r

compileProgram :: Program -> Result
compileProgram p = case p of
  Program topdefs -> do
    let functions = (map transFunc topdefs) ++ getAvailableFunctions
    if length (filter (\x -> name x == "main") functions) == 0 then
      Error "missing function main()" None
    else do
      let initialResult = Success "" (StmtContext [])
      let folded = foldl (\a b -> accumulate a (transTopDef (functions ++ (getContextVars a)) b)) initialResult topdefs
      case folded of
        Error l c -> Error l c
        _ -> do 
          let initialStringValue = "@s- = internal constant [1 x i8] c\"\00\"\n"
          let constants = concatWithSeparator (map (\x -> alias x ++ " = internal constant [" ++ show (1 + length (name x)) ++ " x i8] c\"" ++ name x ++ "\00\"") (vars (context folded))) "\n"
          Success (initialStringValue ++ constants ++ "\n\n" ++ line folded) None

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> Success string (ExpContext string string [])

getCurrentFunction :: [Variable] -> Variable
getCurrentFunction v = case filter isCurrent (filter isFunc v) of
  [] -> EmptyVar
  [x] -> x
  other -> head other

getAvailableFunctions :: [Variable]
getAvailableFunctions = [createFunction "void i32" "printInt",
                          createFunction "void i8*" "printString",
                          createFunction "void" "error",
                          createFunction "i32" "readInt",
                          createFunction "i8*" "readString"]

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
    let ftype = transType type_
    let fName = line $ transIdent ident

    let translatedArgs = map transArg args
    let initial = (EmptyVar, [])
    
    let agumentContexts = map context translatedArgs
    let argumentVariableStartIndex = if line ftype == "void" then 1 else 2
    let argumentVariableEndIndex = if line ftype == "void" then length agumentContexts else (1 + length agumentContexts)
    let argumentVars = combineArrays agumentContexts [argumentVariableStartIndex..argumentVariableEndIndex]
    let retVar = if line ftype == "void" then [] else [createVariable (line ftype) "1" 1]
    let variables = map (\x -> createArgumentVariable (argType (fst x)) ((argName . fst) x) (show (snd x))) argumentVars

    let checkedArguments = foldl (\a b -> accumulateArgs a b) initial variables
    case fst checkedArguments of
      EmptyVar -> do
        -- Mark current function to later check return statement
        let initialVariables = map (\x -> if name x == fName then markFunctionAsCurrent x else x) v
        let argumentsLine = concatArguments $ map line translatedArgs
        let allocArguments = map (\x -> getAllocationLine ("%" ++ alias x) (varType x)) variables
        let storeArguments = map (\x -> getStoreLine x x) variables
      
        let blockCounter = [createBlockCounter]
      
        let blockResult = transBlock block (blockCounter ++ initialVariables ++ retVar ++ variables) 0
      
        case blockResult of 
          Error l c -> Error l c
          Success l c -> do
          
            let hasReturnStatement = length (filter (\x -> varType x == "ret") (vars c))
          
            case line ftype of
              "void" -> do
                let blockLine = line blockResult
                let constantStrings = filter (\x -> varType x == "i8") (vars $ context blockResult)
                let allocAndStoreArguments = "\n" ++ concatWithSeparator allocArguments "\n" ++ "\n" ++ concatWithSeparator storeArguments "\n"
                let returnLine = "\nbr label %return\nreturn:" ++ "\nret void"
                let result = createFunctionDefinition (line ftype) fName argumentsLine (allocAndStoreArguments ++ blockLine) returnLine
                Success result (StmtContext constantStrings)
              other -> case hasReturnStatement of
                0 -> Error ("Missing return statement in function " ++ fName) None
                other -> do
                  let blockLine = line blockResult
                  let constantStrings = filter (\x -> varType x == "i8") (vars $ context blockResult)
                  let retVarType = line ftype
                  let allocAndStoreArguments = "\n" ++ "%1 = alloca " ++ retVarType ++ "\n" ++ concatWithSeparator allocArguments "\n" ++ "\n" ++ concatWithSeparator storeArguments "\n"
                  let finalVar = "%r-"
                  let loadReturnVar = finalVar ++ " = load " ++ line ftype ++ ", " ++ line ftype ++ "* %1\n"
                  let returnLine = "\nbr label %return\nreturn:\n" ++ loadReturnVar ++ "\nret " ++ line ftype ++ " " ++ finalVar
                  let result = createFunctionDefinition (line ftype) fName argumentsLine (allocAndStoreArguments ++ blockLine) returnLine
                  Success result (StmtContext constantStrings)
      other -> Error ("Duplicate definition of " ++ name other ++ " in function " ++ fName) None

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
    let newVars = findAndIncrementBlockCounter v
    let currentBlock = last $ prevBlocks (getBlockCounter newVars)
    let st = translateStatements stmts newVars currentBlock
    let statements = fst st
    case statements of
      [Error l c] -> Error l c
      _ -> do
        let a = concatWithSeparator (map line statements) "\n"
        let retVars = findAndDecrementBlockCounter $ snd st
        Success ("\n" ++ a ++ "\n") (BlockContext retVars)


accumulateArgs :: (Variable, [Variable]) -> Variable -> (Variable, [Variable])
accumulateArgs v a = case checkDuplicate a (snd v) of
  EmptyVar -> (fst v, snd v ++ [a])
  other -> (a, [])


translateStatements :: [Stmt] -> [Variable] -> Integer -> ([Result], [Variable])
translateStatements (x:xs) v l = do
  let result = transStmt x v l
  case result of 
    Error line c -> ([Error line c], v)
    Success line c -> do
      let updatedVars = (vars . context) result
      let res = translateStatements xs updatedVars l
      let f = fst res
      case f of
        [Error a b] -> ([Error a b], updatedVars)
        other -> (result : f, snd res)
translateStatements [] v l = ([], v)

translateExpressions :: [Expr] -> [Variable] -> Integer -> ([Result], [Variable])
translateExpressions (x:xs) v l = do
  let result = transExpr x v l
  case result of 
    Error line c -> ([Error line c], v)
    Success line c -> do
      let updatedVars = (usedVars . context) result
      let res = translateExpressions xs updatedVars l
      let f = fst res
      (result : f, snd res)
translateExpressions [] v l = ([], v)

accumulate :: Result -> Result -> Result
accumulate r1 r2 = case context r2 of
  ExpContext _ _ v -> case r1 of
    Error ln c -> Error (line r1) None
    other -> Success (line r1 ++ "\n" ++ line r2) (StmtContext v)
  StmtContext v -> case r1 of
    Error ln c -> Error (line r1) None
    other -> Success (line r1 ++ "\n" ++ line r2) (StmtContext v)
  None -> case r1 of
    Error ln c -> Error (line r1 ++ "\n" ++ line r2) None
    other -> Error (line r2) None

isBlockStatement :: Stmt -> Bool
isBlockStatement stmt = case stmt of
  BStmt _ -> True
  _ -> False

createRetVar :: Variable
createRetVar = Variable "ret" "ret" "ret" False 1 False True

transStmt :: Stmt -> [Variable] -> Integer -> Result
transStmt x v l = case x of
  Empty -> Success "" (StmtContext v)
  BStmt block -> transBlock block v l
  Decl type_ items -> do
    let tp = line $ transType type_
    let initialResult = Success "" (StmtContext v)
    foldl (\a b -> accumulate a (transItem (getContextVars a) l tp b)) initialResult items
  Ass ident expr -> do
    let e = transExpr expr v l
    case e of 
      Error ln c -> Error ln c
      Success ln c -> do
        let e_ctx = context e
        let i = transIdent ident
        let i_ctx = context i
        let varName = expVar i_ctx
        let declaredVar = getDeclaredVariable varName v
        case declaredVar of
          EmptyVar -> Error ("variable " ++ line i ++ " not declared") None
          _ -> do
            let vType = getType v (alias declaredVar)
            if vType == expType e_ctx then do
              let result = line e ++ "\nstore " ++ expType e_ctx ++ " " ++ expVar e_ctx ++ ", " ++ vType ++ "* %" ++ alias declaredVar
              let newVars = usedVars e_ctx
              Success result (StmtContext newVars)
            else Error ("expression of type " ++ getOriginalType (expType e_ctx) ++ " cannot be assigned to " ++ getOriginalType vType) None
  Incr ident -> do 
    let statement = translateUnaryStatement ident v l
    case statement of
      Error ln c -> Error ln c
      Success ln c -> Success (replace "_op_" "add" ln) c
  Decr ident -> do
    let statement = translateUnaryStatement ident v l
    case statement of
      Error ln c -> Error ln c
      Success ln c -> Success (replace "_op_" "sub" ln) c
  Ret expr -> do 
    let expression = transExpr expr v l
    case expression of 
      Error ln c -> Error ln c
      Success ln c -> do
        let currentFunction = getCurrentFunction $ usedVars c
        let currentFuncType = head $ getFuncType currentFunction
        if currentFuncType /= expType c then
          Error ("function " ++ name currentFunction ++ " must return a value of type " ++ getOriginalType currentFuncType) None
        else do
          let storeRetVar = "store " ++ (expType . context) expression ++ " " ++ (expVar . context) expression ++ ", " ++ (expType . context) expression ++ "* %1"
          let retVar = createRetVar
          Success ("\n" ++ ln ++ "\n" ++ storeRetVar ++ "\nbr label %return\n") (StmtContext (usedVars c ++ [retVar]))
  VRet -> do
    let currentFunction = getCurrentFunction v
    let currentFuncType = head $ getFuncType currentFunction
    case currentFuncType of
      "void" -> Success "br label %return\n" (StmtContext (v ++ [createRetVar]))
      other -> Error ("function " ++ name currentFunction ++ " must return a value of type " ++ getOriginalType currentFuncType) None
  Cond expr stmt -> do
    let expression = transExpr expr v l
    case expression of 
      Error ln c -> Error ln c
      Success ln c -> do
        let newVars = usedVars $ context expression
        let breakVar = nextVarName newVars
        let breakVarNum = nextVarNum newVars

        -- case of "if (false) {}" statement
        case expVar c of 
          "0" -> Success "\n" (StmtContext (usedVars c))
          other -> do
            -- increment level and blockCount if stmt is not a block statement
            let level = if isBlockStatement stmt then l else last $ prevBlocks (getBlockCounter newVars)
            let updatedVars = if isBlockStatement stmt then newVars else findAndIncrementBlockCounter newVars

            let statement = transStmt stmt (updatedVars ++ [createLabel breakVar]) level
            case statement of 
              Error ln c -> Error ln c
              Success ln c -> do
                let tempRetVars = filter (\x -> varType x /= "ret") (vars (context statement))
                let breakVar2 = nextVarName tempRetVars
                let breakVar2Num = nextVarNum tempRetVars
                -- decrement blockCount if stmt is not a block statement
                let retVars = if isBlockStatement stmt then vars c else findAndDecrementBlockCounter tempRetVars
                
                let expLine = (expVar . context) expression
                let break = "\nbr i1 " ++ expLine ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVarNum
                Success (line expression ++ break ++ line statement ++ "\n;<label>:" ++ breakVar2Num) (StmtContext (retVars ++ [createLabel breakVar2]))
  CondElse expr stmt1 stmt2 -> do
    let expression = transExpr expr v l
    case expression of 
      Error ln c -> Error ln c
      Success ln c -> do
        let newVars = (usedVars . context) expression
        let breakVar = nextVarName newVars
        let breakVarNum = nextVarNum newVars

        let level = if isBlockStatement stmt1 then l else last $ prevBlocks (getBlockCounter newVars)
        let updatedVars = if isBlockStatement stmt1 then newVars else findAndIncrementBlockCounter newVars

        let statement1 = transStmt stmt1 (updatedVars ++ [createLabel breakVar]) level
        case statement1 of 
          Error ln c -> Error ln c
          Success ln c -> do
            let stmt1Vars = vars (context statement1)

            let decrementedUpdatedVars = if isBlockStatement stmt1 then stmt1Vars else findAndDecrementBlockCounter stmt1Vars
            let nextLevel = if isBlockStatement stmt2 then level else last $ prevBlocks (getBlockCounter stmt1Vars)
            let tempUpdatedVars = if isBlockStatement stmt2 then decrementedUpdatedVars else findAndIncrementBlockCounter decrementedUpdatedVars

            let breakVar2 = nextVarName tempUpdatedVars
            let breakVar2Num = nextVarNum tempUpdatedVars
            let statement2 = transStmt stmt2 (tempUpdatedVars ++ [createLabel breakVar2]) nextLevel
            case statement2 of 
              Error ln c -> Error ln c
              Success ln c -> do
                let tempRetVars = vars (context statement2)

                let retVars = if isBlockStatement stmt2 then tempRetVars else findAndDecrementBlockCounter tempRetVars

                let expLine = (expVar . context) expression
                let break = "\nbr i1 " ++ expLine ++ ", label " ++ breakVar ++ ", label " ++ breakVar2 ++ "\n;<label>:" ++ breakVarNum
                let stmt1Line = line statement1
                let stmt2Line = line statement2
                let breakVar3 = nextVarName retVars
                let finalBreak = "br label " ++ breakVar3
                let finalBreakNum = nextVarNum retVars
                Success (line expression ++ break ++ "\n" ++ stmt1Line ++ finalBreak ++ "\n;<label>:" ++ breakVar2Num ++ "\n" ++ stmt2Line ++ finalBreak ++ "\n;<label>:" ++ finalBreakNum ++ "\n") 
                  (StmtContext (retVars ++ [createLabel breakVar3]))
  While expr stmt -> do
    let whileLabelVar = nextVarName v
    let whileLabel = "br label " ++ whileLabelVar ++ "\n;<label>:" ++ nextVarNum v ++ "\n"

    let expression = transExpr expr (v ++ [createLabel whileLabelVar]) l
    case expression of 
      Error ln c -> Error ln c
      Success ln c ->
        case expVar c of 
          "0" -> Success "" c
          other -> do
            let expVars = usedVars (context expression)
            let stmtLabelVar = nextVarName expVars
            let stmtLabel = ";<label>:" ++ nextVarNum expVars ++ "\n"

            let level = if isBlockStatement stmt then l else last $ prevBlocks (getBlockCounter expVars)
            let bvpuct = if isBlockStatement stmt then expVars else findAndIncrementBlockCounter expVars

            let statement = transStmt stmt (bvpuct ++ [createLabel stmtLabelVar]) level
            case statement of
              Error ln c -> Error ln c
              Success ln c -> do
                let stmtVars = vars (context statement)
                let continuationVar = nextVarName stmtVars
              
                let grtuct = if isBlockStatement stmt then stmtVars else findAndDecrementBlockCounter stmtVars
              
                let continuationLabel = ";<label>:" ++ nextVarNum grtuct ++ "\n"
                let endStmtBreak = "\nbr label " ++ whileLabelVar ++ "\n"
                let breakLine = "br i1 " ++ expVar (context expression) ++ ", label " ++ stmtLabelVar ++ ", label " ++ continuationVar ++ "\n"
                let returnLine = whileLabel ++ line expression ++ "\n" ++ breakLine ++ stmtLabel ++ line statement ++ endStmtBreak ++ continuationLabel
                Success returnLine (StmtContext (grtuct ++ [createLabel continuationVar]))
  SExp expr -> do
    let expression = transExpr expr v l
    case expression of
      Error ln c -> Error ln c
      Success ln c -> Success ln (StmtContext (usedVars c))

nameVariable :: String -> [Variable] -> String
nameVariable s vars = case last $ prevBlocks (getBlockCounter vars) of
                        1 -> s
                        num -> s ++ show num

getInitLine :: String -> String -> String
getInitLine t n = case t of
  "i32" -> "\n%" ++ n ++ " = alloca " ++ t ++ "\nstore i32 0, i32* %" ++ n
  "i1" -> "\n%" ++ n ++ " = alloca " ++ t ++ "\nstore i1 0, i1* %" ++ n
  "i8*" -> "\n%" ++ n ++ " = bitcast [1 x i8]* @s- to i8*"
  other -> "\n"


transItem :: [Variable] -> Integer -> String -> Item -> Result
transItem v l t x = case x of
  NoInit ident -> do 
    let id = line $ transIdent ident
    let newName = nameVariable id v
    let declaredVar = createDeclaredVariableWithAlias t id newName l
    case checkDuplicate declaredVar v of
      EmptyVar -> Success (getInitLine t newName) (ExpContext t id (v ++ [createDeclaredVariableWithAlias t id newName l]))
      other -> Error ("duplicate definition of variable " ++ newName) None
  Init ident expr -> do 
    let res = transExpr expr v l
    case res of
      Error ln c -> Error ln c
      Success ln c -> if t == expType c then do
          let id = line $ transIdent ident
          let exp = line res
          let ctx = context res
          let vars = usedVars ctx
          let newName = nameVariable id v
          let declaredVar = createDeclaredVariableWithAlias t id newName l
          case checkDuplicate declaredVar v of
            EmptyVar -> if expType c == t then do
                let def = "%" ++ newName ++ " = alloca " ++ expType ctx
                let init = "store " ++ expType ctx ++ " " ++ expVar ctx ++ ", " ++ t ++"* %" ++ newName
                Success (line res ++ "\n" ++ def ++ "\n" ++ init) (ExpContext (expType ctx) newName (vars ++ [declaredVar]))
              else Error ("cannot assign " ++ getOriginalType (expType c) ++ " to variable of type " ++ getOriginalType t) None
            other -> Error ("duplicate definition of variable " ++ newName) None
        else Error ("expression of type " ++ getOriginalType (expType c) ++ " cannot be assigned to " ++ getOriginalType t) None
transType :: Type -> Result
transType x = case x of
  Int -> Success "i32" None
  Str -> Success "i8*" None
  Bool -> Success "i1" None
  Void -> Success "void" None
  Fun type_ types -> Success (line $ transType type_) None

getOriginalType :: String -> String
getOriginalType t = case t of
  "i32" -> "int"
  "i8*" -> "string"
  "i1" -> "bool"
  "void" -> "void"
  other -> "unknown type"

getDeclaredVariable :: String -> [Variable] -> Variable
getDeclaredVariable n vars = do
  let filted = filter (\x -> name x == n) vars
  let prevDeclaredVars = concatMap (\x -> filter (\z -> level z == x) filted) (prevBlocks $ getBlockCounter vars)
  case prevDeclaredVars of
    [] -> EmptyVar
    x -> last x

checkFunctionArgumentTypes :: [String] -> [String] -> Bool
checkFunctionArgumentTypes [] [] = True
checkFunctionArgumentTypes (a:ax) (b:bx) = (a == b) && checkFunctionArgumentTypes ax bx

transExpr :: Expr -> [Variable] -> Integer -> Result
transExpr x v level = case x of
  EVar ident -> do
    let id = transIdent ident
    let name = line id
    let declaredVar = getDeclaredVariable name v
    let tp = getType v (alias declaredVar)
    let var = nextVarName v
    Success (var ++ " = load " ++ tp ++ ", " ++ tp ++ "* %" ++ alias declaredVar) (ExpContext tp var (v ++ [createVariable tp var level]))
  ELitInt integer -> Success "" (ExpContext "i32" (show integer) v)
  ELitTrue -> Success "" (ExpContext "i1" "1" v)
  ELitFalse -> Success "" (ExpContext "i1" "0" v)
  EApp ident exprs -> do
    let resultsVariablesTuple = translateExpressions exprs v level
    let results = fst resultsVariablesTuple
    case results of
      [Error ln c] -> Error ln c
      _ -> do
        let variables = snd resultsVariablesTuple
        let mapedArguments = map (\x -> expType (context x) ++ " " ++ expVar (context x)) results
        let arguments = concatWithSeparator mapedArguments ","
        let lines = map line results
        let expLines = concatWithSeparator lines "\n"
        let id = transIdent ident
        let retVar = nextVarName variables
        let funcType = getFuncType (getFuncVariable variables (line id))
        let returnType = head funcType

        let funcArgTypes = tail funcType
        let passedArgTypes = map (\x -> expType (context x)) results

        let isCorrectCall = if length funcArgTypes == length passedArgTypes then checkFunctionArgumentTypes funcArgTypes passedArgTypes else False
        if isCorrectCall then
          case returnType of
            "void" -> do
              let callLine = expLines ++ "\n" ++ "call " ++ returnType ++ " @" ++ line id ++ "(" ++ arguments ++ ")"
              Success callLine (ExpContext returnType retVar variables)
            _ -> do
              let callLine = expLines ++ "\n" ++ retVar ++ " = call " ++ returnType ++ " @" ++ line id ++ "(" ++ arguments ++ ")"
              let usedVars = variables ++ [createVariable returnType retVar level]
              Success callLine (ExpContext returnType retVar usedVars)
        else Error ("Argument mismatch in function call " ++ line id) None
  EString string -> do
    let stringCount = show $ length (filter (\x -> varType x == "i8") v)
    let globalVar = Variable "i8" string ("@s-" ++ stringCount) False 0 False False
    let name = nextVarName v
    let line = name ++ " = bitcast [" ++ show (1 + length string) ++ " x i8]* " ++ ("@s-" ++ stringCount) ++ " to i8*\n"
    Success line (ExpContext "i8*" name (v ++ [globalVar, createVariable "i8*" name level]))
  Neg expr -> do 
    let expression = transExpr expr v level
    case expression of 
      Error ln c -> Error ln c
      Success ln c -> do
        let expressionVar = expVar c
        case expType c of
          "i32" -> do 
            let expVars = usedVars c
            let negVar = nextVarName expVars
            let negative = negVar ++ " = mul i32 -1, " ++ expressionVar
            Success (line expression ++ "\n" ++ negative) (ExpContext "i32" negVar (expVars ++ [createVariable "i32" negVar level]))
          other -> Error ("cannot negate variable of type " ++ getOriginalType other) None
  Not expr -> do 
    let expression = transExpr expr v level
    case expression of 
      Error ln c -> Error ln c
      Success ln ctx -> do
        let var = nextVarName $ usedVars ctx
        let retLine = var ++ " = xor i1 " ++ expVar ctx ++ ", 1\n"
        case expType ctx of
          "i1" -> Success (line expression ++ "\n" ++ retLine) (ExpContext "i1" var (usedVars ctx ++ [createVariable "i1" var level]))
          other -> Error ("logical negation cannot be applied to variable of type " ++ getOriginalType other) None
  EMul expr1 mulop expr2 -> do
    let translated = translateBinaryExpression expr1 expr2 v level
    case translated of 
      Error ln c -> Error (replace "_op_" (original (context $ transMulOp mulop)) ln) None
      Success ln c -> Success (replace "_op_" (line $ transMulOp mulop) ln) c
  EAdd expr1 addop expr2 -> do
    let translated = translateBinaryExpression expr1 expr2 v level
    case translated of 
      Error ln c -> Error (replace "_op_" (original (context $ transAddOp addop)) ln) None
      Success ln c -> Success (replace "_op_" (line $ transAddOp addop) ln) c
  ERel expr1 relop expr2 -> do
    let l = transExpr expr1 v level
    case l of
      Error ln c -> Error ln c
      Success ln c -> do
        let lExpVar = expVar (context l)
        let o = transRelOp relop
        let newV = usedVars (context l)
        let r = transExpr expr2 newV level
        case r of
          Error ln c -> Error ln c
          Success ln c -> do
            let vars = usedVars (context r)
            if expType (context l) == expType (context r) then
              case expType (context l) of
                "i1" -> do
                  let var = nextVarName vars
                  let newVar = createVariable "i1" var level
                  let ctx = ExpContext "i1" var (vars ++ [newVar])
                  let res = var ++ " = icmp " ++ line o ++ " " ++ expType (context l) ++ " " ++ lExpVar ++ ", " ++ expVar (context r)
                  Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
                "i32" -> do
                  let var = nextVarName vars
                  let newVar = createVariable "i1" var level
                  let ctx = ExpContext "i1" var (vars ++ [newVar])
                  let res = var ++ " = icmp " ++ line o ++ " " ++ expType (context l) ++ " " ++ lExpVar ++ ", " ++ expVar (context r)
                  Success (line l ++ "\n" ++ line r ++ "\n" ++ res) ctx
                tp -> Error ("relational operator " ++ original (context o) ++ " cannot be applied to type " ++ getOriginalType tp) None
            else Error ("type mismatch in relational operator " ++ original (context o)) None
  EAnd expr1 expr2 -> do
    let translated = translateBooleanExpression expr1 expr2 "and" v level
    case translated of 
      Error ln c -> Error (replace "_op_" "&&" ln) None
      Success ln c -> Success (replace "_op_" "and" ln) c
  EOr expr1 expr2 -> do
    let translated = translateBooleanExpression expr1 expr2 "or" v level
    case translated of 
      Error ln c -> Error (replace "_op_" "||" ln) None
      Success ln c -> Success (replace "_op_" "or" ln) c


translateUnaryStatement :: Ident -> [Variable] -> Integer -> Result
translateUnaryStatement ident v l = do 
  let id = line $ transIdent ident
  let declaredVar = getDeclaredVariable id v
  case declaredVar of 
    EmptyVar -> Error (id ++ " not declared ") None
    _ -> do
      let tp = getType v (alias declaredVar)
      case tp of 
        "i32" -> do
          let var = nextVarName v
          let newVars = v ++ [createVariable "i32" var l]
          let load = var ++ " = load i32, " ++ tp ++ "* %" ++ alias declaredVar ++ "\n"
          let incVar = nextVarName newVars
          let increment = incVar ++ " = _op_ i32 " ++ var ++ ", 1\n"
          let res = "store i32 " ++ incVar ++ ", i32* %" ++ alias declaredVar
          let retVars = newVars ++ [createVariable "i32" incVar l]
          Success (load ++ increment ++ res) (StmtContext retVars)
        _-> Error ("unary operators cannot be applied to argument of type " ++ getOriginalType tp) None

translateBinaryExpression :: Expr -> Expr -> [Variable] -> Integer -> Result
translateBinaryExpression expr1 expr2 v level = do
  let l = transExpr expr1 v level
  case l of
    Error ln c -> Error ln c
    Success ln c -> do
      let lvars = usedVars (context l)
      let lvar = createVariable (expType $ context l) (expVar $ context l) level
      let r = transExpr expr2 lvars level
      case r of
        Error ln c -> Error ln c
        Success ln c -> do
          let rvars = usedVars (context r)
          let rvar = createVariable (expType $ context r) (expVar $ context r) level

          if areSameType lvar rvar then do
            let opVarName = nextVarName rvars
            case varType lvar of
              "i8*" -> do
                let res = opVarName ++ " = call i8* @concat(i8* " ++ alias lvar ++ ", i8* " ++ alias rvar ++ ")"
                let retVars = rvars ++ [createVariable (varType lvar) opVarName level]
                let ctx = ExpContext (varType lvar) opVarName retVars
                Success (line l ++ "\n" ++ line r ++ "\n" ++ res ++ "\n") ctx
              "i32" -> do
                let res = opVarName ++ " = _op_ " ++ varType lvar ++ " " ++ alias lvar ++ ", " ++ alias rvar
                let retVars = rvars ++ [createVariable (varType lvar) opVarName level]
                let ctx = ExpContext (varType lvar) opVarName retVars
                Success (line l ++ "\n" ++ line r ++ "\n" ++ res ++ "\n") ctx
              _ -> Error ("binary operator _op_ cannot be applied to operands of type " ++ getOriginalType (varType lvar)) None
          else Error "operands of binary operator _op_ have different types" None

translateBooleanExpression :: Expr -> Expr -> String -> [Variable] -> Integer -> Result
translateBooleanExpression expr1 expr2 op v level = do
  let l = transExpr expr1 v level
  case l of
    Error ln c -> Error ln c
    Success ln c -> do
      let lvars = usedVars (context l)
      let lvar = createVariable (expType $ context l) (expVar $ context l) level
      let exp1Var = expVar c
      let exp1Type = expType c
      let cmpVarName = nextVarName lvars
      let currentLabel = getCurrentLabel lvars
      let updatedVars = lvars ++ [createVariable "i1" cmpVarName level]
      let compare = cmpVarName ++ " = icmp eq i1 " ++ exp1Var ++ ", 0\n"
      case op of
        "and" -> do
          let breakVar = nextVarName updatedVars
          let breakVarNum = nextVarNum updatedVars
          let break = "br i1 " ++ cmpVarName ++ ", label l-" ++ ", label " ++ breakVar ++ "\n"
          let label = ";<label>:" ++ breakVarNum ++ "\n"
          let updatedWithBreakVars = updatedVars ++ [createLabel breakVar]
          let r = transExpr expr2 updatedWithBreakVars level
          case r of 
            Error ln c -> Error ln c
            Success ln c -> do
              let rvars = usedVars (context r)
              let rvar = createVariable (expType $ context r) (expVar $ context r) level
              if (areSameType lvar rvar) && (varType lvar == "i1") then do
                let orVar = nextVarName rvars
                let res = orVar ++ " = _op_ i1 " ++ alias lvar ++ ", " ++ alias rvar
                let retVars = rvars ++ [createVariable "i1" orVar level]
                let phiBreak = nextVarName retVars
                let phiLabel = nextVarNum retVars
                let phiBreakLine = "br label " ++ phiBreak ++ "\n" ++ ";<label>:" ++ phiLabel ++ "\n"
                let retVarsWithPhi = retVars ++ [createLabel phiBreak]
                let phiVar = nextVarName retVarsWithPhi
                let phiLine = phiVar ++ " = phi i1 [0, " ++ currentLabel ++ "], [" ++ orVar ++ ", " ++ getCurrentLabel retVars ++ "]"
                let ctx = ExpContext "i1" phiVar (retVarsWithPhi ++ [createVariable "i1" phiVar level])
                Success (line l ++ "\n" ++ compare ++ (replace "l-" phiBreak break) ++ label ++ line r ++ "\n" ++ res ++ "\n" ++ phiBreakLine ++ phiLine) ctx
              else Error "Boolean operator _op_ can be applied only to boolean variables" None
        "or" -> do
          let breakVar = nextVarName updatedVars
          let breakVarNum = nextVarNum updatedVars
          let break = "br i1 " ++ cmpVarName ++ ", label " ++ breakVar ++ ", label l-\n"
          let label = ";<label>:" ++ breakVarNum ++ "\n"
          let updatedWithBreakVars = updatedVars ++ [createLabel breakVar]
          let r = transExpr expr2 updatedWithBreakVars level
          case r of 
            Error ln c -> Error ln c
            Success ln c -> do
              let rvars = usedVars (context r)
              let rvar = createVariable (expType $ context r) (expVar $ context r) level
              if (areSameType lvar rvar) && (varType lvar == "i1") then do
                let orVar = nextVarName rvars
                let res = orVar ++ " = _op_ i1 " ++ alias lvar ++ ", " ++ alias rvar
                let retVars = rvars ++ [createVariable "i1" orVar level]
                let phiBreak = nextVarName retVars
                let phiLabel = nextVarNum retVars
                let phiBreakLine = "br label " ++ phiBreak ++ "\n" ++ ";<label>:" ++ phiLabel ++ "\n"
                let retVarsWithPhi = retVars ++ [createLabel phiBreak]
                let phiVar = nextVarName retVarsWithPhi
                let phiLine = phiVar ++ " = phi i1 [1, " ++ currentLabel ++ "], [" ++ orVar ++ ", " ++ (getCurrentLabel retVars) ++ "]"
                let ctx = ExpContext "i1" phiVar (retVarsWithPhi ++ [createVariable "i1" phiVar level])
                Success (line l ++ "\n" ++ compare ++ (replace "l-" phiBreak break) ++ label ++ line r ++ "\n" ++ res ++ "\n" ++ phiBreakLine ++ phiLine) ctx
              else Error "Boolean operator _op_ can be applied only to boolean variables" None

getLastLabel :: [Variable] -> String
getLastLabel [] = "%0"
getLastLabel x = if varType (head x) == "label" 
  then name (head x)
  else getLastLabel (tail x)

getCurrentLabel :: [Variable] -> String
getCurrentLabel v = getLastLabel $ reverse v

transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> Success "add" (OperatorContext "+")
  Minus -> Success "sub" (OperatorContext "-")
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> Success "mul" (OperatorContext "*")
  Div -> Success "sdiv" (OperatorContext "/")
  Mod -> Success "srem" (OperatorContext "%")
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> Success "slt" (OperatorContext "<")
  LE -> Success "sle" (OperatorContext "<=")
  GTH -> Success "sgt" (OperatorContext ">")
  GE -> Success "sge" (OperatorContext ">=")
  EQU -> Success "eq" (OperatorContext "==")
  NE -> Success "ne" (OperatorContext "!=")
