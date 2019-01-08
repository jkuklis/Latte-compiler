module TreeConverter where

import Control.Monad.State

import qualified Data.Map as M
import qualified Data.Set as S

import AbsLatte
import ParLatte
import ErrM

import AbstractTree


data ConverterState = ConverterState {
    apps :: [Stmt_],
    typeHints :: TypeHints
    } deriving Show

type CS a = State ConverterState a


startState :: TypeHints -> ConverterState

startState hints = ConverterState {
    apps = [],
    typeHints = hints
}


convert :: String -> TypeHints -> IO Program_

convert input hints = do
    let (Ok prog) = pProgram $ myLexer input
    let prog_ = evalState (progConv prog) $ startState hints
    return prog_


identConv :: Ident -> CS Ident_

identConv (Ident ident) =
    return $ Ident_ ident


progConv :: Program Pos -> CS Program_

progConv (Program _ defs) = do
    defs <- defsConv defs
    return $ Program_ defs


defsConv :: [TopDef Pos] -> CS [TopDef_]

defsConv defs = mapM defConv defs


defConv :: TopDef Pos -> CS TopDef_

defConv (FnDef _ type_ ident args block) = do
    type_ <- typeConv type_
    ident <- identConv ident
    args <- argsConv args
    block <- blockConv block
    return $ FnDef_ type_ ident args block


typesConv :: [Type Pos] -> CS [Type_]

typesConv types = mapM typeConv types


typeConv :: Type Pos -> CS Type_

typeConv type_ =
    case type_ of
        Int _ -> return Int_
        Str _ -> return Str_
        Bool _ -> return Bool_
        Void _ -> return Void_
        Fun _ type_ types -> do
            type_ <- typeConv type_
            types <- typesConv types
            return $ Fun_ type_ types


argsConv :: [Arg Pos] -> CS [Arg_]

argsConv args = mapM argConv args


argConv :: Arg Pos -> CS Arg_

argConv (Arg _ type_ ident) = do
    type_ <- typeConv type_
    ident <- identConv ident
    return $ Arg_ type_ ident


blockConv :: Block Pos -> CS Block_

blockConv (Block _ stmts) = do
    stmts <- stmtsConv stmts
    return $ Block_ stmts


stmtsConv :: [Stmt Pos] -> CS [Stmt_]

stmtsConv stmts = do
    stmts <- mapM stmtConv stmts
    return $ snd $ removeStmts $ concat stmts


-- used to remove code after returns

removeStmts :: [Stmt_] -> (Bool, [Stmt_])

removeStmts stmts =
    let
        removed = foldl gatherStmt (False, []) stmts
    in (fst removed, reverse (snd removed))


gatherStmt :: (Bool, [Stmt_]) -> Stmt_ -> (Bool, [Stmt_])

gatherStmt (ret, sts) st =
    if ret == True
        then (True, sts)
        else case st of
            Empty_ -> (False, sts)

            Ret_ _ -> (True, st:sts)

            VRet_ -> (True, st:sts)

            SExp_ expr ->
                (False, concat [(extractApps expr), sts])

            BStmt_ (Block_ []) -> (False, sts)

            BStmt_ (Block_ stmts) ->
                let
                    removed = removeStmts stmts
                    ret = fst removed
                    block = Block_ $ snd removed
                in (ret, (BStmt_ block):sts)

            Cond_ expr stmt ->
                case snd (removeStmts [stmt]) of
                    [] -> (False, sts)
                    [removed] -> (False, (Cond_ expr removed):sts)

            CondElse_ expr stmt1 stmt2 ->
                let
                    rem1 = removeStmts [stmt1]
                    rem2 = removeStmts [stmt2]
                in case (snd rem1, snd rem2) of
                    ([], []) -> (False, sts)
                    ([], [r2]) -> (False, (Cond_ (Not_ expr) r2):sts)
                    ([r1], []) -> (False, (Cond_ expr r1):sts)
                    ([r1], [r2]) ->
                        let
                            ret = (fst rem1) && (fst rem2)
                        in (ret, (CondElse_ expr r1 r2):sts)

            While_ expr stmt ->
                let
                    removed = removeStmts [stmt]
                    (returned, newSt) = case snd removed of
                        [] -> (False, Empty_)
                        [remSt] -> (fst removed, remSt)
                    newWhile = While_ expr newSt
                in case expr of
                    ELitTrue_ -> (returned, newWhile:sts)
                    _ -> (False, newWhile:sts)

            _ -> (False, st:sts)


-- used for extracting function applications from constant expressions
-- to not lose any side-effects like printing, return them

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
        EStrAdd_ e1 e2 -> doubleExtractApps e1 e2
        ERel_ e1 op e2 -> doubleExtractApps e1 e2
        EAnd_ e1 e2 -> doubleExtractApps e1 e2
        EOr_ e1 e2 -> doubleExtractApps e1 e2


stmtConv :: Stmt Pos -> CS [Stmt_]

stmtConv stmt =
    case stmt of
        Empty _ ->
            return [Empty_]

        BStmt _ block -> do
            block <- blockConv block
            return [BStmt_ block]

        Decl _ type_ items -> do
            type_ <- typeConv type_
            items <- itemsConv items
            return [Decl_ type_ items]

        Ass _ ident expr -> do
            ident <- identConv ident
            expr <- exprConv expr
            return [Ass_ ident expr]

        Incr _ ident -> do
            ident <- identConv ident
            return [Incr_ ident]

        Decr _ ident -> do
            ident <- identConv ident
            return [Decr_ ident]

        Ret _ expr -> do
            expr <- exprConv expr
            return [Ret_ expr]

        VRet _ ->
            return [VRet_]

        Cond _ expr stmt -> do
            expr <- exprConv expr
            apps <- getApps
            stmt <- stmtConv stmt
            case expr of
                ELitTrue_ -> return $ concat [apps, stmt]
                ELitFalse_ -> return $ concat [apps, [Empty_]]
                _ -> return $ concat [apps, [Cond_ expr (singleStmt stmt)]]

        CondElse _ expr stmt1 stmt2 -> do
            expr <- exprConv expr
            apps <- getApps
            stmt1 <- stmtConv stmt1
            stmt2 <- stmtConv stmt2
            case expr of
                ELitTrue_ -> return $ concat [apps, stmt1]
                ELitFalse_ -> return $ concat [apps, stmt2]
                _ ->
                    let cond = CondElse_ expr (singleStmt stmt1) (singleStmt stmt2)
                    in return $ concat [apps, [cond]]

        While _ expr stmt -> do
            expr <- exprConv expr
            apps <- getApps
            stmt <- stmtConv stmt
            case expr of
                ELitFalse_ -> return $ concat [apps, [Empty_]]
                _ -> return [While_ expr (singleStmt (concat [apps, stmt]))]

        SExp _ expr -> do
            expr <- exprConv expr
            gatherApps expr
            getApps


singleStmt :: [Stmt_] -> Stmt_

singleStmt stmts = case stmts of
    [singleSt] -> singleSt
    _ -> BStmt_ $ Block_ stmts


getApps :: CS [Stmt_]

getApps = do
    apps <- gets apps
    modify $ \s -> s { apps = [] }
    return $ reverse apps


itemsConv :: [Item Pos] -> CS [Item_]

itemsConv items = mapM itemConv items


itemConv :: Item Pos -> CS Item_

itemConv item =
    case item of
        NoInit _ ident -> do
            ident <- identConv ident
            return $ NoInit_ ident
        Init _ ident expr -> do
            ident <- identConv ident
            expr <- exprConv expr
            return $ Init_ ident expr


exprsConv :: [Expr Pos] -> CS [Expr_]

exprsConv exprs = mapM exprConv exprs


exprConv :: Expr Pos -> CS Expr_

exprConv expr =
    case expr of
        EVar _ ident -> do
            ident <- identConv ident
            return $ EVar_ ident

        ELitInt _ int ->
            return $ ELitInt_ int

        ELitTrue _ ->
            return $ ELitTrue_

        ELitFalse _ ->
            return $ ELitFalse_

        EApp _ ident exprs -> do
            ident <- identConv ident
            exprs <- exprsConv exprs
            return $ EApp_ ident exprs


        EString _ str ->
            return $ EString_ str

        Neg _ expr -> do
            expr <- exprConv expr
            case expr of
                ELitInt_ int ->
                    return $ ELitInt_ (-int)

                _ ->
                    return $ Neg_ expr

        Not _ expr -> do
            expr <- exprConv expr
            case expr of
                ELitTrue_ ->
                    return ELitFalse_

                ELitFalse_ ->
                    return ELitTrue_

                _ ->
                    return $ Not_ expr

        EMul _ expr1 oper expr2 -> do
            e1 <- exprConv expr1
            e2 <- exprConv expr2
            op <- mulOpConv oper
            case op of
                Times_ -> case (e1, e2) of
                    (ELitInt_ 0, _) -> do
                        gatherApps e2
                        return $ ELitInt_ 0

                    (_, ELitInt_ 0) -> do
                        gatherApps e1
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
                    (_, ELitInt_ 1) -> do
                        gatherApps e2
                        return $ ELitInt_ 0

                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ (int1 `mod` int2)

                    (_, _) ->
                        return $ EMul_ e1 op e2

        EAdd _ expr1 oper expr2 -> do
            e1 <- exprConv expr1
            e2 <- exprConv expr2
            op <- addOpConv oper
            case op of
                Plus_ -> case (e1, e2) of
                    (ELitInt_ int1, ELitInt_ int2) ->
                        return $ ELitInt_ $ int1 + int2

                    (EString_ str1, EString_ str2) -> do
                        return $ EString_ $ (init str1) ++ (tail str2)

                    (_, _) -> do
                        let (Plus (Just pos)) = oper
                        type_ <- findHint pos
                        case type_ of
                            Int_ -> return $ EAdd_ e1 op e2
                            Str_ -> return $ EStrAdd_ e1 e2
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
                    EQU_ _ -> int1 == int2
                    NE_ _ -> int1 /= int2
            e1 <- exprConv expr1
            e2 <- exprConv expr2
            op <- relOpConv oper
            case (e1, e2) of
                (ELitInt_ int1, ELitInt_ int2) ->
                    if rel int1 op int2
                        then return ELitTrue_
                        else return ELitFalse_

                (ELitTrue_, ELitTrue_) -> case op of
                    EQU_ _ -> return ELitTrue_
                    NE_ _ -> return ELitFalse_

                (ELitFalse_, ELitTrue_) -> case op of
                    EQU_ _ -> return ELitFalse_
                    NE_ _ -> return ELitTrue_

                (ELitTrue_, ELitFalse_) -> case op of
                    EQU_ _ -> return ELitFalse_
                    NE_ _ -> return ELitTrue_

                (ELitFalse_, ELitFalse_) -> case op of
                    EQU_ _ -> return ELitTrue_
                    NE_ _ -> return ELitFalse_

                (EString_ str1, EString_ str2) -> case op of
                    EQU_ _ -> if str1 == str2
                        then return ELitTrue_
                        else return ELitFalse_
                    NE_ _ -> if str1 == str2
                        then return ELitFalse_
                        else return ELitTrue_

                (_, _) ->
                    return $ ERel_ e1 op e2

        EAnd _ expr1 expr2 -> do
            e1 <- exprConv expr1
            e2 <- exprConv expr2
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
            e1 <- exprConv expr1
            e2 <- exprConv expr2
            case (e1, e2) of
                (ELitFalse_, ELitFalse_) ->
                    return ELitFalse_

                (ELitTrue_, _) ->
                    return ELitTrue_

                (_, ELitTrue_) ->
                    return ELitTrue_

                (_, _) ->
                    return $ EOr_ e1 e2


-- used for extracting function applications from constant expressions
-- to not lose any side-effects like printing, store them in state

gatherApps :: Expr_ -> CS ()

gatherApps expr =
    let doubleGatherApps e1 e2 = do
        gatherApps e1
        gatherApps e2
    in case expr of
    EApp_ _ _ ->
        modify $ \s -> s { apps = (SExp_ expr) : (apps s) }
    Neg_ expr -> gatherApps expr
    Not_ expr -> gatherApps expr
    EMul_ e1 _ e2 -> doubleGatherApps e1 e2
    EAdd_ e1 _ e2 -> doubleGatherApps e1 e2
    ERel_ e1 _ e2 -> doubleGatherApps e1 e2
    EAnd_ e1 e2 -> doubleGatherApps e1 e2
    EOr_ e1 e2 -> doubleGatherApps e1 e2
    _ -> return ()


mulOpConv :: MulOp Pos -> CS MulOp_

mulOpConv op =
    case op of
        Times _ -> return Times_
        Div _ -> return Div_
        Mod _ -> return Mod_


addOpConv :: AddOp Pos -> CS AddOp_

addOpConv op =
    case op of
        Plus _ -> return Plus_
        Minus _ -> return Minus_


relOpConv :: RelOp Pos -> CS RelOp_

relOpConv op =
    case op of
        LTH _ -> return LTH_
        LE _ -> return LE_
        GTH _ -> return GTH_
        GE _ -> return GE_
        EQU (Just pos) -> do
            type_ <- findHint pos
            return $ EQU_ type_
        NE (Just pos) -> do
            type_ <- findHint pos
            return $ NE_ type_


findHint :: LineChar -> CS Type_

findHint pos = do
    Just type_ <- gets $ M.lookup pos . typeHints
    return type_
