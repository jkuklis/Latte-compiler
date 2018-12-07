module Utility where

import System.IO

import AbsLatte

putErr = hPutStr stderr
putErrLn = hPutStrLn stderr

showId (Ident ident) = ident
