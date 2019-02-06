module Main where

import System.Exit (exitFailure, exitSuccess)

import System.IO

import Analyser (analyse)
import ClassMapConverter (convertClassMap)
import TreeConverter (convert)
import Compiler (compile)

main = do
    input <- getContents
    (continue, typeHints, selfHints, classMap) <- analyse input
    let convertedClassMap = convertClassMap classMap
    -- return ()
    if not continue
        then exitFailure
        else do
            hPutStrLn stderr $ show convertedClassMap
            -- hPutStrLn stderr $ show typeHints
            -- hPutStrLn stderr $ show selfHints

            prog <- convert input typeHints selfHints
            -- hPutStrLn stderr $ show prog

            compile prog convertedClassMap
            exitSuccess
