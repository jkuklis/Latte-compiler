module Analyser where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM

import AnalyserUtility


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
            localsToOuter $ M.toList locals
            cleanLocals
            checkStmts stmts
            setLocals locals
            setOuter outer

        Decl pos dType items ->
            checkDecl dType items

        Ass pos ident expr -> do
            eType <- checkExpr expr
            var <- findVar ident
            case var of
                Just (tPos, type_) ->
                    when (not (cmpTypes type_ eType)) $
                        msgAssign ident pos eType type_ tPos
                Nothing ->
                    msgVarUndefined ident pos

        Incr pos ident -> do
            var <- findVar ident
            case var of
                Just (tPos, type_) ->
                    case type_ of
                        Int tPos -> return ()
                        _ -> msgIncr ident pos tPos type_
                Nothing -> msgVarUndefined ident pos

        Decr pos ident -> checkStmts [Incr pos ident]

        Ret pos expr -> do
            eType <- checkExpr expr
            rType <- gets retType
            if cmpTypes rType eType
                then markReturn
                else msgReturn pos eType

        VRet pos ->
            return ()

        Cond pos expr stmt -> do
            eType <- checkExpr expr
            when (not (cmpTypes eType (Bool defaultPos)))
                $ msgCond pos eType
            checkStmts [stmt]

        CondElse pos expr stmt1 stmt2 ->
            checkStmts [Cond pos expr stmt1, stmt2]

        While pos expr stmt ->
            checkStmts [Cond pos expr stmt]

        SExp pos expr -> do
            checkExpr expr
            return ()

    checkStmts sts


checkDecl :: Type Pos -> [Item Pos] -> AS ()

checkDecl _ [] =
    return ()

checkDecl dType (item:items) = case item of
    Init pos ident expr -> do
        eType <- checkExpr expr
        when (not (cmpTypes dType eType)) $
            msgExpDecl ident pos dType eType
        checkDecl dType $ (NoInit pos ident) : items

    NoInit pos ident -> do
        var <- findLoc ident
        case var of
            Just (vPos, vType) ->
                msgVarDeclared ident pos vPos
            Nothing ->
                addLocal ident pos dType
        checkDecl dType items


checkExpr :: Expr Pos -> AS (Type Pos)

checkExpr expr =
    return defaultType
