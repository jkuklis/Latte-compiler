module Main where

import System.Exit (exitFailure, exitSuccess)

import Analyser (analyse)

main = do
    input <- getContents
    continue <- analyse input
    if continue
        then exitSuccess
        else exitFailure
