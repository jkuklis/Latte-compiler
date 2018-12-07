{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Analyser where

import Control.Monad.State
import Control.Monad.Except

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM

import Utility


type Pos = Maybe (Int, Int)

type FunMap = M.Map Ident (Pos, Type Pos, [Arg Pos])

type VarMap = M.Map Ident (Pos, Type Pos)

data AnalysisState = AnalysisState {
    continue :: Bool,
    errors :: [String],
    funMap :: FunMap,
    outVarMap :: VarMap,
    locVarMap :: VarMap,
    retType :: Type Pos,
    ret :: Bool
    } deriving Show

type AS a = State AnalysisState a

startState = AnalysisState {
    continue = True,
    errors = [],
    funMap = M.empty,
    outVarMap = M.empty,
    locVarMap = M.empty,
    retType = Int (Just(0,0)),
    ret = False
}


msgRedefined :: String -> Ident -> Pos -> Pos -> String

msgRedefined what (Ident ident) (Just (line, char)) (Just (prevLine, prevChar))
    = what ++ " " ++ ident ++ " redefined!\n"
    ++ "Redefined in line " ++ (show line)
    ++ " (at pos " ++ (show char) ++ ")"
    ++ ", previously defined in line " ++ (show prevLine)
    ++ " (at pos " ++ (show prevChar) ++ ")\n"


msgFunDefined :: Ident -> Pos -> Pos -> String

msgFunDefined ident pos prevPos =
    msgRedefined "Function" ident pos prevPos


msgSameArg :: Ident -> Pos -> Pos -> String

msgSameArg ident pos prevPos =
    msgRedefined "Argument" ident pos prevPos


addError :: String -> AS ()

addError error =
    modify $ \s -> s { errors = (error : (errors s)), continue = False }


addPrototype :: Ident -> Pos -> Type Pos -> [Arg Pos] -> AS ()

addPrototype ident pos type_ args =
    modify $ \s -> s { funMap = M.insert ident (pos, type_, args) (funMap s) }


setRetType :: Type Pos -> AS ()

setRetType type_ =
    modify $ \s -> s { retType = type_, ret = False }


addOuter :: Ident -> Pos -> Type Pos -> AS ()

addOuter ident pos type_ =
    modify $ \s -> s { outVarMap = M.insert ident (pos, type_) (outVarMap s) }


addArgs :: [Arg Pos] -> AS ()

addArgs [] =
    return ()

addArgs (arg:args) = do
    let Arg pos type_ ident = arg
    outer <- gets $ M.lookup ident . outVarMap
    case outer of
        Just (prevPos, _) -> do
            addError $ msgSameArg ident pos prevPos
        Nothing -> do
            addOuter ident pos type_
    addArgs args


getPrototypes :: [TopDef Pos] -> AS ()

getPrototypes [] =
    return ()

getPrototypes (def:defs) = do
    let FnDef pos type_ ident args block = def
    fun <- gets $ M.lookup ident . funMap
    case fun of
        Just (prevPos, _, _) -> do
            addError $ msgFunDefined ident pos prevPos
        Nothing -> do
            addPrototype ident pos type_ args
    getPrototypes defs


checkFunctions :: [TopDef Pos] -> AS ()

checkFunctions [] =
    return ()

checkFunctions (def:defs) = do
    let FnDef pos type_ ident args block = def
    setRetType type_
    addArgs args
    -- checkBlock block
    checkFunctions defs



analyse :: String -> IO Bool

analyse input =
    let tokens = myLexer input in
        case pProgram tokens of
            Bad error -> do
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
