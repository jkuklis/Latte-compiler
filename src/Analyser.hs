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
                -- putErrLn $ show state
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
                    when (type_ == Int pos && args == []) $ addPrototype ident pos type_ args
                else
                    addPrototype ident pos type_ args
    getPrototypes defs


checkFunctions :: [TopDef Pos] -> AS ()

checkFunctions [] =
    return ()

checkFunctions (def:defs) = do
    let FnDef pos type_ ident args (Block bPos stmts) = def
    setRetType type_
    cleanOuter
    addArgs args
    checkBlock stmts
    case type_ of
        Void tPos ->
            return ()
        _ -> do
            returned <- gets ret
            return ()
            -- when (not returned) $ msgNoReturn ident pos
    checkFunctions defs


checkBlock :: [Stmt Pos] -> AS ()

checkBlock [] =
    return ()

checkBlock (st:sts) = do
    case st of
        Empty pos ->
            return ()

        BStmt pos (Block bPos stmts) -> do
            locals <- gets locVarMap
            outer <- gets outVarMap
            localsToOuter $ M.toList locals
            cleanLocals
            checkBlock stmts
            setLocals locals
            setOuter outer

        Decl pos type_ items ->
            return ()

        Ass pos ident expr ->
            return ()

        Incr pos ident -> do
            loc <- gets $ M.lookup ident . locVarMap
            out <- gets $ M.lookup ident . outVarMap
            case loc of
                Just (tPos, type_) -> case type_ of
                    Int tPos -> return ()
                    _ -> msgIncr ident pos tPos type_
                Nothing -> case out of
                    Just (tPos, type_) -> case type_ of
                        Int tPos -> return ()
                        _ -> msgIncr ident pos tPos type_
                    Nothing -> msgVarUndefined ident pos

        Decr pos ident -> checkBlock [Incr pos ident]

        Ret pos expr ->
            return ()

        VRet pos ->
            return ()

        Cond pos expr stmt ->
            return ()

        CondElse pos expr stmt1 stmt2 ->
            return ()

        While pos expr stmt ->
            return ()

        SExp pos expr ->
            return ()

    checkBlock sts

checkExpr :: Expr Pos -> AS (Type Pos)

checkExpr expr =
    return defaultType
