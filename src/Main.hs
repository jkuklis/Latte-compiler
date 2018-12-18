module Main where

import System.Exit (exitFailure, exitSuccess)

import Analyser (analyse)
import TreeConverter (convert)

main = do
    input <- getContents
    continue <- analyse input
    if not continue
        then exitFailure
        else do
            convert input
            exitSuccess
