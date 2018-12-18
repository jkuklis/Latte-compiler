module TreeConverter where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM


type Pos = Maybe (Int, Int)


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

data Type_ = Int_ | Str_ | Bool_ | Void_ | Fun_ Type_ [Type_]
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
    | ERel_ Expr_ RelOp_ Expr_
    | EAnd_ Expr_ Expr_
    | EOr_ Expr_ Expr_
  deriving (Eq, Ord, Show, Read)

data AddOp_ = Plus_ | Minus_
  deriving (Eq, Ord, Show, Read)

data MulOp_ = Times_ | Div_ | Mod_
  deriving (Eq, Ord, Show, Read)

data RelOp_ = LTH_ | LE_ | GTH_ | GE_ | EQU_ | NE_
  deriving (Eq, Ord, Show, Read)


identNoPos (Ident ident) = Ident_ ident


progNoPos :: Program Pos -> Program_

progNoPos (Program _ defs) =
    let defs_ = defsNoPos defs
    in Program_ defs_


defsNoPos :: [TopDef Pos] -> [TopDef_]

defsNoPos defs = map defNoPos defs


defNoPos :: TopDef Pos -> TopDef_

defNoPos (FnDef _ fType ident args block) =
    let
        fType_ = typeNoPos fType
        ident_ = identNoPos ident
        args_ = argsNoPos args
        block_ = blockNoPos block
    in FnDef_ fType_ ident_ args_ block_


typesNoPos :: [Type Pos] -> [Type_]

typesNoPos types = map typeNoPos types


typeNoPos :: Type Pos -> Type_

typeNoPos type_ =
    case type_ of
        Int _ -> Int_
        Str _ -> Str_
        Bool _ -> Bool_
        Void _ -> Void_
        Fun _ type_ types ->
            Fun_ (typeNoPos type_) $ typesNoPos types


argsNoPos :: [Arg Pos] -> [Arg_]

argsNoPos args = map argNoPos args


argNoPos :: Arg Pos -> Arg_

argNoPos (Arg _ type_ ident) =
    Arg_ (typeNoPos type_) $ identNoPos ident


blockNoPos :: Block Pos -> Block_

blockNoPos (Block _ stmts) =
    Block_ $ reverse $ stmtsNoPos stmts


stmtsNoPos :: [Stmt Pos] -> [Stmt_]

stmtsNoPos stmts = map stmtNoPos stmts


stmtNoPos :: Stmt Pos -> Stmt_

stmtNoPos stmt =
    case stmt of
        Empty _ ->
            Empty_

        BStmt _ block ->
            BStmt_ $ blockNoPos block

        Decl _ type_ items ->
            Decl_ (typeNoPos type_) $ itemsNoPos items

        Ass _ ident expr ->
            Ass_ (identNoPos ident) $ exprNoPos expr

        Incr _ ident ->
            Incr_ $ identNoPos ident

        Decr _ ident ->
            Decr_ $ identNoPos ident

        Ret _ expr ->
            Ret_ $ exprNoPos expr

        VRet _ ->
            VRet_

        Cond _ expr stmt ->
            Cond_ (exprNoPos expr) $ stmtNoPos stmt

        CondElse _ expr stmt1 stmt2 ->
            CondElse_ (exprNoPos expr) (stmtNoPos stmt1) $ stmtNoPos stmt2

        While _ expr stmt ->
            While_ (exprNoPos expr) $ stmtNoPos stmt

        SExp _ expr ->
            SExp_ $ exprNoPos expr


itemsNoPos :: [Item Pos] -> [Item_]

itemsNoPos items = map itemNoPos items


itemNoPos :: Item Pos -> Item_

itemNoPos item =
    case item of
        NoInit _ ident -> NoInit_ $ identNoPos ident
        Init _ ident expr -> Init_ (identNoPos ident) $ exprNoPos expr


exprsNoPos :: [Expr Pos] -> [Expr_]

exprsNoPos exprs = map exprNoPos exprs


exprNoPos :: Expr Pos -> Expr_

exprNoPos expr =
    case expr of
        EVar _ ident -> EVar_ $ identNoPos ident
        ELitInt _ int -> ELitInt_ int
        ELitTrue _ -> ELitTrue_
        ELitFalse _ -> ELitFalse_
        EApp _ ident exprs -> EApp_ (identNoPos ident) $ exprsNoPos exprs
        EString _ str -> EString_ str
        Neg _  expr -> Neg_ $ exprNoPos expr
        Not _  expr -> Not_ $ exprNoPos expr
        EMul _  expr1 op expr2 ->
            EMul_ (exprNoPos expr1) (mulOpNoPos op) $ exprNoPos expr2
        EAdd _  expr1 op expr2 ->
            EAdd_ (exprNoPos expr1) (addOpNoPos op) $ exprNoPos expr2
        ERel _  expr1 op expr2 ->
            ERel_ (exprNoPos expr1) (relOpNoPos op) $ exprNoPos expr2
        EAnd _  expr1 expr2 ->
            EAnd_ (exprNoPos expr1) $ exprNoPos expr2
        EOr _  expr1 expr2 ->
            EOr_ (exprNoPos expr1) $ exprNoPos expr2


mulOpNoPos :: MulOp Pos -> MulOp_

mulOpNoPos op =
    case op of
        Times _ -> Times_
        Div _ -> Div_
        Mod _ -> Mod_


addOpNoPos :: AddOp Pos -> AddOp_

addOpNoPos op =
    case op of
        Plus _ -> Plus_
        Minus _ -> Minus_


relOpNoPos :: RelOp Pos -> RelOp_

relOpNoPos op =
    case op of
        LTH _ -> LTH_
        LE _ -> LE_
        GTH _ -> GTH_
        GE _ -> GE_
        EQU _ -> EQU_
        NE _ -> NE_


convert :: String -> IO String

convert input = do
    let (Ok prog) = pProgram $ myLexer input
    let prog_ = progNoPos prog
    putStrLn $ show prog_
    return input
