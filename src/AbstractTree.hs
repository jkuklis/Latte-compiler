module AbstractTree where

import qualified Data.Map as M

import AbsLatte


type LineChar = (Int, Int)

type Pos = Maybe LineChar

type TypeHints = M.Map LineChar Type_


newtype Ident_ = Ident_ String deriving (Eq, Ord, Show, Read)
data Program_ = Program_ [TopDef_]
  deriving (Eq, Ord, Show, Read)

data TopDef_ = FnDef_ Type_ Ident_ [Arg_] Block_
  deriving (Eq, Ord, Show, Read)

data Arg_ = Arg_ Type_ Ident_
  deriving (Eq, Ord, Show, Read)

data Block_ = Block_ [Stmt_]
  deriving (Eq, Ord, Show, Read)

data Stmt_
    = Empty_
    | BStmt_ Block_
    | Decl_ Type_ [Item_]
    | Ass_ Ident_ Expr_
    | Incr_ Ident_
    | Decr_ Ident_
    | Ret_ Expr_
    | VRet_
    | Cond_ Expr_ Stmt_
    | CondElse_ Expr_ Stmt_ Stmt_
    | While_ Expr_ Stmt_
    | SExp_ Expr_
  deriving (Eq, Ord, Show, Read)

data Item_ = NoInit_ Ident_ | Init_ Ident_ Expr_
  deriving (Eq, Ord, Show, Read)

data Type_ = Int_ | Str_ | Bool_ | Void_ | Fun_ Type_ [Type_] | None_
  deriving (Eq, Ord, Show, Read)

data Expr_
    = EVar_ Ident_
    | ELitInt_ Integer
    | ELitTrue_
    | ELitFalse_
    | EApp_ Ident_ [Expr_]
    | EString_ String
    | Neg_ Expr_
    | Not_ Expr_
    | EMul_ Expr_ MulOp_ Expr_
    | EAdd_ Expr_ AddOp_ Expr_
    | EStrAdd_ Expr_ Expr_
    | ERel_ Expr_ RelOp_ Expr_
    | EAnd_ Expr_ Expr_
    | EOr_ Expr_ Expr_
  deriving (Eq, Ord, Show, Read)

data AddOp_ = Plus_ | Minus_
  deriving (Eq, Ord, Show, Read)

data MulOp_ = Times_ | Div_ | Mod_
  deriving (Eq, Ord, Show, Read)

data RelOp_ = LTH_ | LE_ | GTH_ | GE_ | EQU_ Type_ | NE_ Type_
  deriving (Eq, Ord, Show, Read)