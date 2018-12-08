module Analyser where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM

import AnalyserUtility
import AnalyserErrors


analyse :: String -> IO Bool

analyse input =
    let tokens = myLexer input in
        case pProgram tokens of
            Bad error -> do
                putErrLn "ERROR\n"
                putErrLn "Failure to parse program!"
                putErrLn error
                return False

            Ok (Program _ defs) -> do
                let statePrototypes = execState (getPrototypes defs) startState
                let state = execState (checkFunctions defs) statePrototypes
                if continue state
                    then
                        putErrLn "OK\n"
                    else do
                        putErrLn "ERROR\n"
                        putErr $ unlines $ errors state
                return $ continue state


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
    checkStmts stmts
    case type_ of
        Void tPos ->
            return ()
        _ -> do
            returned <- gets ret
            return ()
            when (not returned) $ msgNoReturn ident pos
    checkFunctions defs


addArgs :: [Arg Pos] -> AS ()

addArgs [] =
    return ()

addArgs (arg:args) = do
    let Arg pos type_ ident = arg
    outer <- gets $ M.lookup ident . outVarMap
    case outer of
        Just (prevPos, _) ->
            msgSameArg ident pos prevPos
        Nothing ->
            addOuter ident pos type_
    addArgs args


checkStmts :: [Stmt Pos] -> AS ()

checkStmts [] =
    return ()

checkStmts (st:sts) = do
    case st of
        Empty pos ->
            return ()

        BStmt pos (Block bPos stmts) -> do
            locals <- gets locVarMap
            outer <- gets outVarMap
            isOutermost <- innerBlock
            localsToOuter $ M.toList locals
            checkStmts stmts
            outerBlock isOutermost
            setLocals locals
            setOuter outer

        Decl pos dType items ->
            checkDecl dType items

        Ass pos ident expr -> do
            eType <- checkExpr expr
            var <- findVar ident
            case var of
                Just (prevPos, vType) ->
                    checkTypes eType vType $ msgAssign ident pos vType prevPos
                Nothing ->
                    msgVarUndeclared ident pos

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

        Decr pos ident -> checkStmts [Incr pos ident]

        Ret pos expr -> do
            eType <- checkExpr expr
            rType <- gets retType
            case rType of
                Void fPos ->
                    msgNotVoidReturn pos

                _ -> case eType of
                    Nothing ->
                        return ()
                    Just (eType) ->
                        if cmpTypes rType eType
                            then tryMarkReturn
                            else msgReturn pos eType

        VRet pos -> do
            rType <- gets retType
            case rType of
                Void fPos ->
                    return ()
                _ ->
                    msgVoidReturn pos

        Cond pos expr stmt -> do
            eType <- checkExpr expr
            checkTypes eType (Bool defaultPos) $ msgCond pos
            isOutermost <- innerBlock
            checkStmts [stmt]
            outerBlock isOutermost

        CondElse pos expr stmt1 stmt2 ->
            checkStmts [Cond pos expr stmt1, stmt2]

        While pos expr stmt ->
            checkStmts [Cond pos expr stmt]

        SExp pos expr -> do
            checkExpr expr
            return ()

    checkStmts sts


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

    ELitInt pos int ->
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

        return $ Just $ Bool pos

    Not pos expr -> do

        return $ Just $ Bool pos

    EMul pos e1 op e2 ->  do

        return $ Just $ Int pos

    EAdd pos e1 op e2 ->  do

        return $ Just $ Int pos

    ERel pos e1 op e2 ->  do

        return $ Just $ Bool pos

    EAnd pos e1 e2 ->  do

        return $ Just $ Bool pos

    EOr pos e1 e2 ->  do

        return $ Just $ Bool pos


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


checkExprs :: [Expr Pos] -> AS ()

checkExprs [] =
    return ()

checkExprs (expr:exprs) = do
    checkExpr expr
    checkExprs exprs
