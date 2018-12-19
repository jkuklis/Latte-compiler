module TreeConverter where

import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

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


data ConverterState = ConverterState {
    funs :: S.Set Ident_,
    vars :: S.Set Ident_
    } deriving Show


startState = ConverterState {
    funs = S.empty,
    vars = S.empty
}

type CS a = State ConverterState a


identNoPos :: Ident -> CS Ident_

identNoPos (Ident ident) =
    return $ Ident_ ident


progNoPos :: Program Pos -> CS Program_

progNoPos (Program _ defs) = do
    defs <- defsNoPos defs
    return $ Program_ defs


defsNoPos :: [TopDef Pos] -> CS [TopDef_]

defsNoPos defs = mapM defNoPos defs


defNoPos :: TopDef Pos -> CS TopDef_

defNoPos (FnDef _ type_ ident args block) = do
    type_ <- typeNoPos type_
    ident <- identNoPos ident
    args <- argsNoPos args
    block <- blockNoPos block
    return $ FnDef_ type_ ident args block


typesNoPos :: [Type Pos] -> CS [Type_]

typesNoPos types = mapM typeNoPos types


typeNoPos :: Type Pos -> CS Type_

typeNoPos type_ =
    case type_ of
        Int _ -> return Int_
        Str _ -> return Str_
        Bool _ -> return Bool_
        Void _ -> return Void_
        Fun _ type_ types -> do
            type_ <- typeNoPos type_
            types <- typesNoPos types
            return $ Fun_ type_ types


argsNoPos :: [Arg Pos] -> CS [Arg_]

argsNoPos args = mapM argNoPos args


argNoPos :: Arg Pos -> CS Arg_

argNoPos (Arg _ type_ ident) = do
    type_ <- typeNoPos type_
    ident <- identNoPos ident
    return $ Arg_ type_ ident


blockNoPos :: Block Pos -> CS Block_

blockNoPos (Block _ stmts) = do
    stmts <- stmtsNoPos stmts
    return $ Block_ stmts


stmtsNoPos :: [Stmt Pos] -> CS [Stmt_]

stmtsNoPos stmts = do
    stmts <- mapM stmtNoPos stmts
    mapM reduceStmt (removeStmts stmts)


removeStmts :: [Stmt_] -> [Stmt_]

removeStmts stmts =
    reverse $ snd $ foldl gatherStmt (False, []) stmts


gatherStmt :: (Bool, [Stmt_]) -> Stmt_ -> (Bool, [Stmt_])

gatherStmt (ret, stmts) st =
    if ret == True
        then (True, stmts)
        else case st of
            Empty_ -> (False, stmts)

            Ret_ _ -> (True, st:stmts)

            VRet_ -> (True, st:stmts)

            SExp_ expr ->
                (False, concat [(extractApps expr), stmts])

            _ -> (False, st:stmts)


extractApps :: Expr_ -> [Stmt_]

extractApps expr =
    let
        doubleExtractApps e1 e2 = concat [(extractApps e2), (extractApps e1)]
    in case expr of
        EVar_ _ -> []
        ELitInt_ _ -> []
        ELitTrue_ -> []
        ELitFalse_ -> []
        EApp_ _ _ -> [SExp_ expr]
        EString_ _ -> []
        Neg_ expr -> extractApps expr
        Not_ expr -> extractApps expr
        EMul_ e1 op e2 -> doubleExtractApps e1 e2
        EAdd_ e1 op e2 -> doubleExtractApps e1 e2
        ERel_ e1 op e2 -> doubleExtractApps e1 e2
        EAnd_ e1 e2 -> doubleExtractApps e1 e2
        EOr_ e1 e2 -> doubleExtractApps e1 e2


reduceStmt :: Stmt_ -> CS Stmt_

reduceStmt stmt =
    case stmt of

        _ -> return stmt


stmtNoPos :: Stmt Pos -> CS Stmt_

stmtNoPos stmt =
    case stmt of
        Empty _ ->
            return Empty_

        BStmt _ block -> do
            block <- blockNoPos block
            return $ BStmt_ block

        Decl _ type_ items -> do
            type_ <- typeNoPos type_
            items <- itemsNoPos items
            return $ Decl_ type_ items

        Ass _ ident expr -> do
            ident <- identNoPos ident
            expr <- exprNoPos expr
            return $ Ass_ ident expr

        Incr _ ident -> do
            ident <- identNoPos ident
            return $ Incr_ ident

        Decr _ ident -> do
            ident <- identNoPos ident
            return $ Decr_ ident

        Ret _ expr -> do
            expr <- exprNoPos expr
            return $ Ret_ expr

        VRet _ ->
            return VRet_

        Cond _ expr stmt -> do
            expr <- exprNoPos expr
            stmt <- stmtNoPos stmt
            case expr of
                ELitTrue_ -> return stmt
                ELitFalse_ -> return Empty_
                _ -> return $ Cond_ expr stmt

        CondElse _ expr stmt1 stmt2 -> do
            expr <- exprNoPos expr
            stmt1 <- stmtNoPos stmt1
            stmt2 <- stmtNoPos stmt2
            case expr of
                ELitTrue_ -> return stmt1
                ELitFalse_ -> return stmt2
                _ -> return $ CondElse_ expr stmt1 stmt2

        While _ expr stmt -> do
            expr <- exprNoPos expr
            case expr of
                ELitFalse_ -> return Empty_
                _ -> do
                    stmt <- stmtNoPos stmt
                    return $ While_ expr stmt

        SExp _ expr -> do
            expr <- exprNoPos expr
            return $ SExp_ expr


itemsNoPos :: [Item Pos] -> CS [Item_]

itemsNoPos items = mapM itemNoPos items


itemNoPos :: Item Pos -> CS Item_

itemNoPos item =
    case item of
        NoInit _ ident -> do
            ident <- identNoPos ident
            return $ NoInit_ ident
        Init _ ident expr -> do
            ident <- identNoPos ident
            expr <- exprNoPos expr
            return $ Init_ ident expr


exprsNoPos :: [Expr Pos] -> CS [Expr_]

exprsNoPos exprs = mapM exprNoPos exprs


exprNoPos :: Expr Pos -> CS Expr_

exprNoPos expr =
    case expr of
        EVar _ ident -> do
            ident <- identNoPos ident
            return $ EVar_ ident

        ELitInt _ int ->
            return $ ELitInt_ int

        ELitTrue _ ->
            return $ ELitTrue_

        ELitFalse _ ->
            return $ ELitFalse_

        EApp _ ident exprs -> do
            ident <- identNoPos ident
            exprs <- exprsNoPos exprs
            return $ EApp_ ident exprs


        EString _ str ->
            return $ EString_ str

        Neg _ expr -> do
            expr <- exprNoPos expr
            case expr of
                ELitInt_ int ->
                    return $ ELitInt_ (-int)

                _ ->
                    return $ Neg_ expr

        Not _ expr -> do
            expr <- exprNoPos expr
            case expr of
                ELitTrue_ ->
                    return ELitFalse_

                ELitFalse_ ->
                    return ELitTrue_

                _ ->
                    return $ Not_ expr

        EMul _ expr1 oper expr2 -> do
            e1 <- exprNoPos expr1
            e2 <- exprNoPos expr2
            op <- mulOpNoPos oper
            case op of
                Times_ -> case (e1, e2) of
                    -- add functions to invoke!
                    (ELitInt_ 0, _) ->
                        return $ ELitInt_ 0

                    (_, ELitInt_ 0) ->
                        return $ ELitInt_ 0

                    (ELitInt_ 1, _) ->
                        return e2

                    (_, ELitInt_ 1) ->
                        return e1

                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ $ int1 * int2

                    (_, _) ->
                        return $ EMul_ e1 Times_ e2

                Div_ -> case (e1, e2) of
                    (_, ELitInt_ 1) ->
                        return e1

                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ (int1 `div` int2)

                    (_, _) ->
                        return $ EMul_ e1 op e2

                Mod_ -> case (e1, e2) of
                    (_, ELitInt_ 1) ->
                        return $ ELitInt_ 0

                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ (int1 `mod` int2)

                    (_, _) ->
                        return $ EMul_ e1 op e2

        EAdd _ expr1 oper expr2 -> do
            e1 <- exprNoPos expr1
            e2 <- exprNoPos expr2
            op <- addOpNoPos oper
            case op of
                Plus_ -> case (e1, e2) of
                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ $ int1 + int2

                    (EString_ str1, EString_ str2) ->
                        return $ EString_ $ str1 ++ str2

                    (_, _) ->
                        return $ EAdd_ e1 op e2
                Minus_ -> case (e1, e2) of
                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ $ int1 - int2

                    (_, _) ->
                        return $ EAdd_ e1 op e2

        ERel _ expr1 oper expr2 -> do
            let
                rel int1 op int2 = case op of
                    LTH_ -> int1 < int2
                    LE_ -> int1 <= int2
                    GTH_ -> int1 > int2
                    GE_ -> int1 >= int2
            e1 <- exprNoPos expr1
            e2 <- exprNoPos expr2
            op <- relOpNoPos oper
            case (e1, e2) of
                (ELitInt_ int1, ELitInt_ int2) ->
                    if rel int1 op int2
                        then return ELitTrue_
                        else return ELitFalse_

                (ELitTrue_, ELitTrue_) -> case op of
                    EQU_ -> return ELitTrue_
                    NE_ -> return ELitFalse_

                (ELitFalse_, ELitTrue_) -> case op of
                    EQU_ -> return ELitFalse_
                    NE_ -> return ELitTrue_

                (ELitTrue_, ELitFalse_) -> case op of
                    EQU_ -> return ELitFalse_
                    NE_ -> return ELitTrue_

                (ELitFalse_, ELitFalse_) -> case op of
                    EQU_ -> return ELitTrue_
                    NE_ -> return ELitFalse_

                (EString_ str1, EString_ str2) -> case op of
                    EQU_ -> if str1 == str2
                        then return ELitTrue_
                        else return ELitFalse_
                    NE_ -> if str1 == str2
                        then return ELitFalse_
                        else return ELitTrue_

                (_, _) ->
                    return $ ERel_ e1 op e2

        EAnd _ expr1 expr2 -> do
            e1 <- exprNoPos expr1
            e2 <- exprNoPos expr2
            case (e1, e2) of
                (ELitTrue_, ELitTrue_) ->
                    return ELitTrue_

                (ELitFalse_, _) ->
                    return ELitFalse_

                (_, ELitFalse_) ->
                    return ELitFalse_

                (_, _) ->
                    return $ EAnd_ e1 e2

        EOr _ expr1 expr2 -> do
            e1 <- exprNoPos expr1
            e2 <- exprNoPos expr2
            case (e1, e2) of
                (ELitFalse_, ELitFalse_) ->
                    return ELitFalse_

                (ELitTrue_, _) ->
                    return ELitTrue_

                (_, ELitTrue_) ->
                    return ELitTrue_

                (_, _) ->
                    return $ EOr_ e1 e2


mulOpNoPos :: MulOp Pos -> CS MulOp_

mulOpNoPos op =
    case op of
        Times _ -> return Times_
        Div _ -> return Div_
        Mod _ -> return Mod_


addOpNoPos :: AddOp Pos -> CS AddOp_

addOpNoPos op =
    case op of
        Plus _ -> return Plus_
        Minus _ -> return Minus_


relOpNoPos :: RelOp Pos -> CS RelOp_

relOpNoPos op =
    case op of
        LTH _ -> return LTH_
        LE _ -> return LE_
        GTH _ -> return GTH_
        GE _ -> return GE_
        EQU _ -> return EQU_
        NE _ -> return NE_


convert :: String -> IO String

convert input = do
    let (Ok prog) = pProgram $ myLexer input
    let prog_ = evalState (progNoPos prog) startState
    putStrLn $ show prog_
    return input
