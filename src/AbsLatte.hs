

module AbsLatte where

-- Haskell module generated by the BNF converter

-- Custom Data types
data Variable = Variable {varType :: String, name :: String, alias :: String, isFunc :: Bool, level :: Integer, isUsed :: Bool, isGenerated :: Bool}
                | BlockCounter {varType :: String, name :: String, alias :: String, isFunc :: Bool, level :: Integer, isUsed :: Bool, isGenerated :: Bool, count :: Integer, prevBlocks :: [Integer]}
                | FuncVariable {varType :: String, name :: String, alias :: String, isFunc :: Bool, level :: Integer, isUsed :: Bool, isGenerated :: Bool, isCurrent :: Bool}
                | ReturnVariable {varType :: String, name :: String, alias :: String, isFunc :: Bool, level :: Integer, isUsed :: Bool, isGenerated :: Bool, isReachable :: Bool}
                | EmptyVar
  deriving (Eq, Ord, Show, Read)

data Context 
    = None
    | ProgramContext {funcNames :: [String]}
    | FuncContext {retType :: String, arguments :: [String]}
    | ArgContext {argType :: String, argName :: String}
    | BlockContext {vars :: [Variable]}
    | StmtContext {vars :: [Variable], number :: Integer}
    | ExpContext {expType :: String, expVar :: String, usedVars :: [Variable]}
    | OperatorContext {original :: String}
    | ErrorContext {stmtNumber :: Integer, function :: String}
  deriving (Eq, Ord, Show, Read)
data Result 
    = Success {line :: String, context :: Context}
    | Error {line :: String, context :: Context} 
  deriving (Eq, Ord, Show, Read)
-- End of custom data types

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)
  
data Program = Program [TopDef]
  deriving (Eq, Ord, Show, Read)

data TopDef = FnDef Type Ident [Arg] Block
  deriving (Eq, Ord, Show, Read)

data Arg = Arg Type Ident
  deriving (Eq, Ord, Show, Read)

data Block = Block [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = Empty
    | BStmt Block
    | Decl Type [Item]
    | Ass Ident Expr
    | Incr Ident
    | Decr Ident
    | Ret Expr
    | VRet
    | Cond Expr Stmt
    | CondElse Expr Stmt Stmt
    | While Expr Stmt
    | SExp Expr
  deriving (Eq, Ord, Show, Read)

data Item = NoInit Ident | Init Ident Expr
  deriving (Eq, Ord, Show, Read)

data Type = Int | Str | Bool | Void | Fun Type [Type]
  deriving (Eq, Ord, Show, Read)

data Expr
    = EVar Ident
    | ELitInt Integer
    | ELitTrue
    | ELitFalse
    | EApp Ident [Expr]
    | EString String
    | Neg Expr
    | Not Expr
    | EMul Expr MulOp Expr
    | EAdd Expr AddOp Expr
    | ERel Expr RelOp Expr
    | EAnd Expr Expr
    | EOr Expr Expr
  deriving (Eq, Ord, Show, Read)

data AddOp = Plus | Minus
  deriving (Eq, Ord, Show, Read)

data MulOp = Times | Div | Mod
  deriving (Eq, Ord, Show, Read)

data RelOp = LTH | LE | GTH | GE | EQU | NE
  deriving (Eq, Ord, Show, Read)
