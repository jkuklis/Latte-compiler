module Compiler where

import Control.Monad.State

import AbstractTree


data CompilerState = CompilerState {
    code :: [String]
    }

type CS a = State CompilerState a

startState = CompilerState {
    code = []
    }


compDefs :: [TopDef_] -> CS ()

compDefs defs =
    return ()


compile :: Program_ -> IO ()

compile (Program_ defs) = do
    -- putStrLn $ show prog
    let a = evalState (compDefs defs) startState
    return ()
