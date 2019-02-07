module AbstractTree where

import qualified Data.Map as M
import qualified Data.Set as S

import AbsLatte


type LineChar = (Int, Int)

type Pos = Maybe LineChar

type TypeHints = M.Map LineChar Type_

type SelfHints = S.Set LineChar


newtype Ident_ = Ident_ String deriving (Eq, Ord, Show, Read)
data Program_ = Program_ [TopDef_]
  deriving (Eq, Ord, Show, Read)

data TopDef_
    = FnDef_ Type_ Ident_ [Arg_] Block_
    | ClDef_ Ident_ ClBlock_
    | ClInher_ Ident_ Ident_ ClBlock_
  deriving (Eq, Ord, Show, Read)

data Arg_ = Arg_ Type_ Ident_
  deriving (Eq, Ord, Show, Read)

data ClBlock_ = ClBlock_ [ClMember_]
  deriving (Eq, Ord, Show, Read)

data ClMember_
    = ClAttr_ Type_ Ident_
    | ClFun_ Type_ Ident_ [Arg_] Block_
  deriving (Eq, Ord, Show, Read)

data Block_ = Block_ [Stmt_]
  deriving (Eq, Ord, Show, Read)

data Stmt_
    = Empty_
    | BStmt_ Block_
    | Decl_ Type_ [Item_]
    | Ass_ Ident_ Expr_
    | ElemAss_ Ident_ Expr_ Expr_
    | AttrAss_ Ident_ Ident_ Ident_ Expr_
    | SelfAtAss_ Ident_ Expr_
    | Incr_ Ident_
    | Decr_ Ident_
    | SelfIncr_ Ident_
    | SelfDecr_ Ident_
    | Ret_ Expr_
    | VRet_
    | Cond_ Expr_ Stmt_
    | CondElse_ Expr_ Stmt_ Stmt_
    | While_ Expr_ Stmt_
    | SExp_ Expr_
  deriving (Eq, Ord, Show, Read)

data Item_ = NoInit_ Ident_ | Init_ Ident_ Expr_
  deriving (Eq, Ord, Show, Read)

data Type_
    = Int_
    | Str_
    | Bool_
    | Void_
    | Class_ Ident_
    | Array_ Type_
    | Fun_ Type_ [Type_]
    | None_
  deriving (Eq, Ord, Show, Read)

data Expr_
    = EVar_ Ident_
    | ELitInt_ Integer
    | ELitTrue_
    | ELitFalse_
    | EApp_ Ident_ [Expr_]
    | EString_ String
    | EElem_ Ident_ Expr_
    | ENull_ Ident_
    | EArrayNew_ Expr_
    | ENew_ Ident_
    | EASelf_ Ident_
    | EMSelf_ Ident_ [Expr_]
    | EAttr_ Ident_ Ident_ Ident_
    | EMethod_ Ident_ Ident_ Ident_ [Expr_]
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
