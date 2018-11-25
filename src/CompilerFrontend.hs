module CompilerFrontend where

import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Except
import Control.Monad.Reader
import Text.Printf

type BlockDepth = Integer

type EnvElem = Type
type TypeEnv a = Map.Map Ident a

data VarEnvElem = VarEnvElem {
    type_ :: Type,
    blockDepth :: BlockDepth
} deriving Show

-- instance Typeable VarEnvElem where
--     getType varEnvElem = type_ varEnvElem

type VarEnv = TypeEnv VarEnvElem
type FunEnv = TypeEnv EnvElem

data Env = Env {
    varEnv :: VarEnv,
    funEnv :: FunEnv,
    actBlockDepth :: BlockDepth,
    actReturnType :: Type
} deriving Show

type Eval a = ReaderT Env (ExceptT String IO) a

checkProgram :: Program -> IO ()
checkProgram p = do
        result <- unfoldProgram p
        printf "llvm-as -o %s %s"
        return ()

-- functionIsDuplicate :: [TopDef] -> Eval
-- functionIsDuplicate [] = False
-- functionIsDuplicate defs =

unfoldIdent :: Ident -> Eval ()
unfoldIdent x = case x of
  Ident string -> failure x
unfoldProgram :: Program -> Eval Program
unfoldProgram (Program topdefs) = do unfoldTopDef topdefs
                                  $ Program topdefs

unfoldTopDef :: TopDef -> Eval TopDef
unfoldTopDef (FnDef type_ ident args block) = do unfoldArg args
                                                 unfoldBlock block
                                                 unfoldIdent ident

unfoldArg :: Arg -> Result
unfoldArg x = case x of
  Arg type_ ident -> failure x
unfoldBlock :: Block -> Result
unfoldBlock x = case x of
  Block stmts -> failure x
unfoldStmt :: Stmt -> Result
unfoldStmt x = case x of
  Empty -> failure x
  BStmt block -> failure x
  Decl type_ items -> failure x
  Ass ident expr -> failure x
  Incr ident -> failure x
  Decr ident -> failure x
  Ret expr -> failure x
  VRet -> failure x
  Cond expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  SExp expr -> failure x
unfoldItem :: Item -> Result
unfoldItem x = case x of
  NoInit ident -> failure x
  Init ident expr -> failure x
unfoldType :: Type -> Result
unfoldType x = case x of
  Int -> failure x
  Str -> failure x
  Bool -> failure x
  Void -> failure x
  Fun type_ types -> failure x
unfoldExpr :: Expr -> Result
unfoldExpr x = case x of
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  EApp ident exprs -> failure x
  EString string -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
unfoldAddOp :: AddOp -> Result
unfoldAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
unfoldMulOp :: MulOp -> Result
unfoldMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
unfoldRelOp :: RelOp -> Result
unfoldRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x
