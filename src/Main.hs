module Main where

import System.Exit (exitFailure, exitSuccess)

import Analyser (analyse)
import TreeConverter (convert)
import Compiler (compile)

main = do
    input <- getContents
    (continue, typeHints) <- analyse input
    if not continue
        then exitFailure
        else do
            prog <- convert input typeHints
            compile prog
            exitSuccess
