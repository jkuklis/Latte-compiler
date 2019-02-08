module Main where

import System.Exit (exitFailure, exitSuccess)

import System.IO

import Analyser (analyse)
import ClassMapConverter (convertClassMap)
import TreeConverter (convert)
import Compiler (compile)

main = do
    input <- getContents
    (continue, typeHints, classMap) <- analyse input
    -- hPutStrLn stderr $ show input
    let convertedClassMap = convertClassMap classMap
    if not continue
        then exitFailure
        else do
            -- hPutStrLn stderr $ show convertedClassMap
            -- hPutStrLn stderr $ show typeHints

            prog <- convert input typeHints
            -- hPutStrLn stderr $ show prog

            compile prog convertedClassMap
            exitSuccess
