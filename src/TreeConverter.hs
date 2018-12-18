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


identRemovePos (Ident ident) = Ident_ ident


progRemovePos :: Program Pos -> Program_

progRemovePos (Program _ defs) =
    let defs_ = defsRemovePos defs
    in Program_ defs_


defsRemovePos :: [TopDef Pos] -> [TopDef_]

defsRemovePos defs = map defRemovePos defs


defRemovePos :: TopDef Pos -> TopDef_

defRemovePos (FnDef _ fType ident args block) =
    let
        fType_ = typeRemovePos fType
        ident_ = identRemovePos ident
        args_ = argsRemovePos args
        block_ = blockRemovePos block
    in FnDef_ fType_ ident_ args_ block_


typesRemovePos :: [Type Pos] -> [Type_]

typesRemovePos types = map typeRemovePos types


typeRemovePos :: Type Pos -> Type_

typeRemovePos type_ =
    case type_ of
        Int _ -> Int_
        Str _ -> Str_
        Bool _ -> Bool_
        Void _ -> Void_
        Fun _ type_ types ->
            Fun_ (typeRemovePos type_) $ typesRemovePos types


argsRemovePos :: [Arg Pos] -> [Arg_]

argsRemovePos args = map argRemovePos args


argRemovePos :: Arg Pos -> Arg_

argRemovePos (Arg _ type_ ident) =
    Arg_ (typeRemovePos type_) $ identRemovePos ident


blockRemovePos :: Block Pos -> Block_

blockRemovePos (Block _ stmts) =
    Block_ $ reverse $ stmtsRemovePos stmts


stmtsRemovePos :: [Stmt Pos] -> [Stmt_]

stmtsRemovePos stmts = map stmtRemovePos stmts


stmtRemovePos :: Stmt Pos -> Stmt_

stmtRemovePos stmt =
    case stmt of
        Empty _ ->
            Empty_

        BStmt _ block ->
            BStmt_ $ blockRemovePos block

        Decl _ type_ items ->
            Decl_ (typeRemovePos type_) $ itemsRemovePos items

        Ass _ ident expr ->
            Ass_ (identRemovePos ident) $ exprRemovePos expr

        Incr _ ident ->
            Incr_ $ identRemovePos ident

        Decr _ ident ->
            Decr_ $ identRemovePos ident

        Ret _ expr ->
            Ret_ $ exprRemovePos expr

        VRet _ ->
            VRet_

        Cond _ expr stmt ->
            Cond_ (exprRemovePos expr) $ stmtRemovePos stmt

        CondElse _ expr stmt1 stmt2 ->
            CondElse_ (exprRemovePos expr) (stmtRemovePos stmt1) $ stmtRemovePos stmt2

        While _ expr stmt ->
            While_ (exprRemovePos expr) $ stmtRemovePos stmt

        SExp _ expr ->
            SExp_ $ exprRemovePos expr


itemsRemovePos :: [Item Pos] -> [Item_]

itemsRemovePos items = map itemRemovePos items


itemRemovePos :: Item Pos -> Item_

itemRemovePos item =
    case item of
        NoInit _ ident -> NoInit_ $ identRemovePos ident
        Init _ ident expr -> Init_ (identRemovePos ident) $ exprRemovePos expr


exprsRemovePos :: [Expr Pos] -> [Expr_]

exprsRemovePos exprs = map exprRemovePos exprs


exprRemovePos :: Expr Pos -> Expr_

exprRemovePos expr =
    case expr of
        EVar _ ident -> EVar_ $ identRemovePos ident
        ELitInt _ int -> ELitInt_ int
        ELitTrue _ -> ELitTrue_
        ELitFalse _ -> ELitFalse_
        EApp _ ident exprs -> EApp_ (identRemovePos ident) $ exprsRemovePos exprs
        EString _ str -> EString_ str
        Neg _  expr -> Neg_ $ exprRemovePos expr
        Not _  expr -> Not_ $ exprRemovePos expr
        EMul _  expr1 op expr2 ->
            EMul_ (exprRemovePos expr1) (mulOpRemovePos op) $ exprRemovePos expr2
        EAdd _  expr1 op expr2 ->
            EAdd_ (exprRemovePos expr1) (addOpRemovePos op) $ exprRemovePos expr2
        ERel _  expr1 op expr2 ->
            ERel_ (exprRemovePos expr1) (relOpRemovePos op) $ exprRemovePos expr2
        EAnd _  expr1 expr2 ->
            EAnd_ (exprRemovePos expr1) $ exprRemovePos expr2
        EOr _  expr1 expr2 ->
            EOr_ (exprRemovePos expr1) $ exprRemovePos expr2


mulOpRemovePos :: MulOp Pos -> MulOp_

mulOpRemovePos op =
    case op of
        Times _ -> Times_
        Div _ -> Div_
        Mod _ -> Mod_


addOpRemovePos :: AddOp Pos -> AddOp_

addOpRemovePos op =
    case op of
        Plus _ -> Plus_
        Minus _ -> Minus_


relOpRemovePos :: RelOp Pos -> RelOp_

relOpRemovePos op =
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
    let prog_ = progRemovePos prog
    putStrLn $ show prog_
    return input
