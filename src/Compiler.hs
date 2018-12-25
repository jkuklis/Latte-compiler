module Compiler where

import Control.Monad.State

import AbstractTree
import CompilerUtility


compDefs :: [TopDef_] -> CS ()

compDefs defs = forM_ defs compDef


compDef :: TopDef_ -> CS ()

compDef (FnDef_ type_ ident args block) = do
    addFun ident
    clearArgs
    addArgs args
    addBlock block
    checkVoidRet type_
    moveFrame

compile :: Program_ -> IO ()

compile (Program_ defs) = do
    -- putStrLn $ show defs
    let state = execState (compDefs defs) startState
    -- putStrLn $ show state
    putStrLn $ unlines $ reverse $ code state
