module Main where

import System.Exit (exitFailure, exitSuccess)

import Analyser (analyse)
import TreeConverter (convert)
import Compiler (compile)

main = do
    input <- getContents
    continue <- analyse input
    if not continue
        then exitFailure
        else do
            prog <- convert input
            compile prog
            exitSuccess
