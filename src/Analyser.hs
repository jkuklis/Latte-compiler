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
    outerVarMap :: VarMap,
    localVarMap :: VarMap
    } deriving Show

type AS a = State AnalysisState a

startState = AnalysisState {
    continue = True,
    errors = [],
    funMap = M.empty,
    outerVarMap = M.empty,
    localVarMap = M.empty

}


msgFunDefined :: Ident -> Pos -> Pos -> String

msgFunDefined (Ident ident) (Just (line, char)) (Just (prevLine, prevChar)) =
    "Function " ++ ident ++ " redefined!\n"
    ++ "Defined in line " ++ (show line)
    ++ " (at pos " ++ (show char) ++ ")"
    ++ ", previously defined in line " ++ (show prevLine)
    ++ " (at pos " ++ (show prevChar) ++ ")\n"


addError :: String -> AS ()

addError error = do
    modify $ \s -> s { errors = (error : (errors s)) }
    modify $ \s -> s { continue = False }


addPrototype :: Ident -> Pos -> Type Pos -> [Arg Pos] -> AS ()

addPrototype ident pos type_ args =
    modify $ \s -> s { funMap = M.insert ident (pos, type_, args) (funMap s) }


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


checkVariables :: [TopDef Pos] -> AS ()

checkVariables [] =
    return ()

checkVariables (def:defs) = do
    let FnDef pos type_ ident args block = def
    fun <- gets $ M.lookup ident . funMap


    checkVariables defs



analyse :: String -> IO Bool

analyse input =
    let tokens = myLexer input in
        case pProgram tokens of
            Bad error -> do
                putErrLn "Failure to parse program!"
                putErrLn error
                return False

            Ok (Program _ defs) -> do
                let state = execState (getPrototypes defs) startState
                -- let state = execState (checkVariables defs) state
                if continue state
                    then
                        putErrLn "OK\n"
                    else do
                        putErrLn "ERROR\n"
                        putErr $ unlines $ errors state
                return $ continue state
