module Analyser where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM

import AnalyserUtility
import AnalyserErrors
import AbstractTree


analyse :: String -> IO (Bool, TypeHints)

analyse input =
    let tokens = myLexer input in
        case pProgram tokens of
            Bad error -> do
                putErrLn "ERROR\n"
                putErrLn "Failure to parse program!"
                putErrLn $ error ++ "\n"
                return (False, M.empty)

            Ok (Program _ defs) -> do
                -- putErrLn $ show defs
                let statePrototypes = execState (getPrototypes defs) startState
                let state = execState (checkFunctions defs) statePrototypes
                if continue state
                    then
                        putErrLn "OK\n"
                    else do
                        putErrLn "ERROR\n"
                        putErr $ unlines $ reverse $ errors state
                -- putErrLn $ show state
                return (continue state, typeHints state)


getPrototypes :: [TopDef Pos] -> AS ()

getPrototypes [] = do
    main <- gets $ M.lookup (Ident "main") . funMap
    when (main == Nothing) $ msgNoMain


getPrototypes (def:defs) = do
    let FnDef pos type_ ident args block = def
    fun <- gets $ M.lookup ident . funMap
    case fun of
        Just (prevPos, _, _) ->
            msgFunDefined ident pos prevPos
        Nothing -> do
            if ident == Ident "main"
                then do
                    when (type_ /= Int pos) $ msgMainType type_ pos
                    when (args /= []) $ msgMainArgs pos
                    when (type_ == Int pos && args == []) $
                        addPrototype ident pos type_ args
                else
                    addPrototype ident pos type_ args
    getPrototypes defs


checkFunctions :: [TopDef Pos] -> AS ()

checkFunctions [] =
    return ()

checkFunctions (def:defs) = do
    let FnDef pos type_ ident args (Block bPos stmts) = def
    setRetType type_
    setCur ident
    cleanVars
    addArgs args
    returned <- checkStmts stmts
    case type_ of
        Void tPos ->
            return ()
        _ -> do
            when (not returned) $ msgNoReturn ident pos
    checkFunctions defs


addArgs :: [Arg Pos] -> AS ()

addArgs [] =
    return ()

addArgs (arg:args) = do
    let Arg pos type_ ident = arg
    case type_ of
        Void _ ->
            msgVoidArg pos ident
        _ ->
            return ()
    outer <- gets $ M.lookup ident . outVarMap
    case outer of
        Just (prevPos, _) ->
            msgSameArg ident pos prevPos
        Nothing ->
            addOuter ident pos type_
    addArgs args


checkStmts :: [Stmt Pos] -> AS Bool

checkStmts [] =
    return False

checkStmts (st:sts) = do
    returnedSt <- checkStmt st
    returnedStmts <- checkStmts sts
    return $ returnedSt || returnedStmts


checkStmt :: Stmt Pos -> AS Bool

checkStmt st = case st of
    Empty pos ->
        return False

    BStmt pos (Block bPos stmts) -> do
        locals <- gets locVarMap
        outer <- gets outVarMap
        localsToOuter $ M.toList locals
        returned <- checkStmts stmts
        setLocals locals
        setOuter outer
        return returned

    Decl pos dType items -> do
        case dType of
            Void _ ->
                msgVoidVar pos
            _ ->
                checkDecl dType items
        return False

    Ass pos ident expr -> do
        eType <- checkExpr expr
        var <- findVar ident
        case var of
            Just (prevPos, vType) ->
                checkTypes eType vType $ msgAssign ident pos vType prevPos
            Nothing ->
                msgVarUndeclared ident pos
        return False

    Incr pos ident -> do
        var <- findVar ident
        case var of
            Just (tPos, type_) ->
                case type_ of
                    Int tPos ->
                        return ()
                    _ ->
                        msgIncr ident pos tPos type_
            Nothing -> msgVarUndeclared ident pos
        return False

    Decr pos ident -> checkStmt $ Incr pos ident

    Ret pos expr -> do
        eType <- checkExpr expr
        rType <- gets retType
        case rType of
            Void fPos -> do
                msgNotVoidReturn pos
                return False
            _ -> case eType of
                Nothing ->
                    return False
                Just (eType) ->
                    if cmpTypes rType eType
                        then return True
                        else do
                            msgReturn pos eType
                            return False

    VRet pos -> do
        rType <- gets retType
        case rType of
            Void fPos ->
                return True
            _ -> do
                msgVoidReturn pos
                return False

    Cond pos expr stmt -> checkStmt $ CondElse pos expr stmt (Empty pos)

    CondElse pos expr stmt1 stmt2 -> do
        eType <- checkExpr expr
        checkTypes eType defaultBool $ msgCond pos
        ret1 <- checkStmt stmt1
        ret2 <- checkStmt stmt2
        constCond <- constantBool expr
        case constCond of
            Just True ->
                return ret1
            Just False ->
                return ret2
            Nothing ->
                return $ ret1 && ret2

    While pos expr stmt -> do
        checkStmt $ Cond pos expr stmt

    SExp pos expr -> do
        checkExpr expr
        return False

checkTypes :: Maybe (Type Pos) -> Type Pos -> (Type Pos -> AS ()) -> AS ()

checkTypes eType dType action = case eType of
    Nothing ->
        return ()
    Just (eType) ->
        when (not (cmpTypes dType eType)) (action eType)


checkDecl :: Type Pos -> [Item Pos] -> AS ()

checkDecl _ [] =
    return ()

checkDecl dType (item:items) = case item of
    Init pos ident expr -> do
        eType <- checkExpr expr
        checkTypes eType dType $ msgExpDecl ident pos dType
        checkDecl dType $ (NoInit pos ident) : items

    NoInit pos ident -> do
        var <- findLoc ident
        case var of
            Just (vPos, vType) ->
                msgVarDeclared ident pos vPos
            Nothing ->
                addLocal ident pos dType
        checkDecl dType items


checkExpr :: Expr Pos -> AS (Maybe (Type Pos))

checkExpr expr = case expr of
    EVar pos ident -> do
        var <- findVar ident
        case var of
            Just (vPos, vType) ->
                return $ Just vType
            Nothing -> do
                msgVarUndefined ident pos
                return Nothing

    ELitInt pos int -> do
        let minInt = -2147483648
        let maxInt = 2147483647
        if minInt > int
            then msgIntTooSmall pos int
            else when (maxInt < int) $ msgIntTooBig pos int
        return $ Just $ Int pos

    ELitTrue pos ->
        return $ Just $ Bool pos

    ELitFalse pos ->
        return $ Just $ Bool pos

    EApp pos ident exprs -> do
        fun <- gets $ M.lookup ident . funMap
        case fun of
            Just (fPos, fType, args) -> do
                checkArgs ident pos args exprs
                return $ Just fType
            Nothing -> do
                msgFunUndefined ident pos
                return Nothing

    EString pos str ->
        return $ Just $ Str pos

    Neg pos expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultInt $ msgNeg pos
        return $ Just $ Int pos

    Not pos expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultBool $ msgNot pos
        return $ Just $ Bool pos

    EMul pos e1 op e2 ->  do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultInt $ msgMul pos 1
        checkTypes eType2 defaultInt $ msgMul pos 2
        return $ Just $ Int pos

    EAdd pos e1 op e2 ->  do
        checkAdd expr

    ERel pos e1 op e2 ->  do
        checkRel expr
        return $ Just $ Bool pos

    EAnd pos e1 e2 ->  do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultBool $ msgAnd pos 1
        checkTypes eType2 defaultBool $ msgAnd pos 2
        return $ Just $ Bool pos

    EOr pos e1 e2 ->  do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultBool $ msgOr pos 1
        checkTypes eType2 defaultBool $ msgOr pos 2
        return $ Just $ Bool pos


checkExprs :: [Expr Pos] -> AS ()

checkExprs [] =
    return ()

checkExprs (expr:exprs) = do
    checkExpr expr
    checkExprs exprs


checkArgs :: Ident -> Pos -> [Arg Pos] -> [Expr Pos] -> AS ()

checkArgs _ _ [] [] =
    return ()

checkArgs ident pos args [] =
    msgTooFewArgs ident pos args

checkArgs ident pos [] exprs = do
    msgTooManyArgs ident pos exprs
    checkExprs exprs

checkArgs ident pos (arg:args) (expr:exprs) = do
    let Arg _ aType aIdent = arg
    eType <- checkExpr expr
    checkTypes eType aType $ msgArgType pos ident aIdent aType
    checkArgs ident pos args exprs


checkAdd :: Expr Pos -> AS (Maybe (Type Pos))

checkAdd (EAdd pos e1 op e2) =
    case op of
        Plus (Just oPos) -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            case eType1 of
                Just (Int ePos) -> do
                    case eType2 of
                        Just (Int ePos) ->
                            return ()
                        Nothing ->
                            return ()
                        Just eType2 ->
                            msgAddInt pos 2 eType2
                    placeHint oPos Int_
                    return $ Just $ Int pos

                Just (Str ePos) -> do
                    case eType2 of
                        Just (Str ePos) ->
                            return ()
                        Nothing ->
                            return ()
                        Just eType2 ->
                            msgAddStr pos 2 eType2
                    placeHint oPos Str_
                    return $ Just $ Str pos

                Just eType1 -> case eType2 of
                    Just (Int ePos) -> do
                        msgAddInt pos 1 eType1
                        return $ Just $ Int pos

                    Just (Str ePos) -> do
                        msgAddStr pos 1 eType1
                        return $ Just $ Str pos

                    Nothing -> do
                        msgAddType pos
                        return Nothing

                Nothing -> case eType2 of
                    Just (Int ePos) ->
                        return $ Just $ Int pos

                    Just (Str ePos) ->
                        return $ Just $ Str pos

                    Nothing -> do
                        msgAddType pos
                        return Nothing

        Minus oPos -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            checkTypes eType1 defaultInt $ msgMinus pos 1
            checkTypes eType2 defaultInt $ msgMinus pos 2
            return $ Just $ Int pos


checkRel :: Expr Pos -> AS ()

checkRel (ERel pos e1 op e2) =
    case op of
        EQU oPos -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            case eType1 of
                Just eType1 -> do
                    case eType1 of
                        Int (Just rPos) -> placeHint rPos Int_
                        Bool (Just rPos) -> placeHint rPos Bool_
                        Str (Just rPos) -> placeHint rPos Str_
                    checkTypes eType2 eType1 $ msgEqType pos eType1

                Nothing ->
                    return ()

        NE oPos ->
            checkRel (ERel pos e1 (EQU oPos) e2)

        _ -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            checkTypes eType1 defaultInt $ msgRelInt pos 1
            checkTypes eType2 defaultInt $ msgRelInt pos 2


constantBool :: Expr Pos -> AS (Maybe Bool)

constantBool expr = case expr of
    EVar _ _ ->
        return Nothing

    ELitTrue _ ->
        return $ Just True

    ELitFalse _ ->
        return $ Just False

    EApp _ _ _ ->
        return Nothing

    Not _ expr -> do
        con <- constantBool expr
        case con of
            Just bool ->
                return $ Just $ not bool
            Nothing ->
                return Nothing

    ERel _ e1 op e2 ->
        let
            sameVal con1 con2 =
                case (con1, con2) of
                    (Just v1, Just v2) ->
                        return $ Just $ v1 == v2
                    _ ->
                        return Nothing

        in case op of
            EQU _ -> do
                Just eType <- checkExpr e1
                case eType of
                    Int _ -> do
                        con1 <- constantInt e1
                        con2 <- constantInt e1
                        sameVal con1 con2

                    Bool _ -> do
                        con1 <- constantBool e1
                        con2 <- constantBool e1
                        sameVal con1 con2

                    Str _ -> do
                        con1 <- constantStr e1
                        con2 <- constantStr e2
                        sameVal con1 con2

            NE pos -> do
                con <- constantBool (ERel pos e1 (EQU pos) e2)
                case con of
                    Just bool ->
                        return $ Just $ not bool
                    Nothing ->
                        return Nothing

            _ -> do
                con1 <- constantInt e1
                con2 <- constantInt e2
                sameVal con1 con2

    EAnd _ e1 e2 -> do
        con1 <- constantBool e1
        con2 <- constantBool e2
        case (con1, con2) of
            (Just True, Just True) ->
                return $ Just True
            (Just False, _) ->
                return $ Just False
            (_, Just False) ->
                return $ Just False
            _ ->
                return Nothing

    EOr _ e1 e2 -> do
        con1 <- constantBool e1
        con2 <- constantBool e2
        case (con1, con2) of
            (Just False, Just False) ->
                return $ Just False
            (Just True, _) ->
                return $ Just True
            (_, Just True) ->
                return $ Just True
            _ ->
                return Nothing

    _ ->
        return Nothing


constantInt :: Expr Pos -> AS (Maybe Integer)

constantInt expr = case expr of
    EVar _ _ ->
        return Nothing

    ELitInt _ int ->
        return $ Just int

    EApp _ _ _ ->
        return Nothing

    Neg _ expr -> do
        con <- constantInt expr
        case con of
            Just int ->
                return $ Just $ negate int
            _ ->
                return Nothing

    EMul _ e1 op e2 -> do
        con1 <- constantInt e1
        con2 <- constantInt e2
        case (con1, con2) of
            (Just i1, Just i2) ->
                case op of
                    Times _ ->
                        return $ Just $ i1 * i2
                    Div pos ->
                        if i2 == 0
                            then do
                                msgDivZero pos
                                return Nothing
                            else
                                return $ Just $ i1 `div` i2
                    Mod pos ->
                        if i2 == 0
                            then do
                                msgModZero pos
                                return Nothing
                            else
                                return $ Just $ i1 `mod` i2
            _ ->
                return Nothing

    EAdd _ e1 op e2 -> do
        con1 <- constantInt e1
        con2 <- constantInt e2
        case (con1, con2) of
            (Just i1, Just i2) ->
                case op of
                    Plus _ ->
                        return $ Just $ i1 + i2
                    Minus _ ->
                        return $ Just $ i1 - i2
            _ ->
                return Nothing

    _ ->
        return Nothing


constantStr :: Expr Pos -> AS (Maybe String)

constantStr expr = case expr of
    EVar _ _ ->
        return Nothing

    EApp _ _ _->
        return Nothing

    EString _ str ->
        return $ Just str

    EAdd pos e1 op e2 ->  do
        con1 <- constantStr e1
        con2 <- constantStr e2
        case (con1, con2) of
            (Just str1, Just str2) ->
                return $ Just $ str1 ++ str2
            _ ->
                return Nothing

    _ ->
        return Nothing
