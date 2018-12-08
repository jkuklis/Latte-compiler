module AnalyserUtility where

import System.IO
import Control.Monad.State
import qualified Data.Map as M

import AbsLatte


putErr = hPutStr stderr
putErrLn = hPutStrLn stderr


type Pos = Maybe (Int, Int)

type FunMap = M.Map Ident (Pos, Type Pos, [Arg Pos])

type VarMap = M.Map Ident (Pos, Type Pos)

data AnalysisState = AnalysisState {
    continue :: Bool,
    errors :: [String],
    funMap :: FunMap,
    outVarMap :: VarMap,
    locVarMap :: VarMap,
    retType :: Type Pos,
    ret :: Bool
    } deriving Show

type AS a = State AnalysisState a

startState = AnalysisState {
    continue = True,
    errors = [],
    funMap = M.empty,
    outVarMap = M.empty,
    locVarMap = M.empty,
    retType = defaultType,
    ret = True
}


defaultType = Int $ Just $ (0,0)


addError :: String -> AS ()

addError error =
    modify $ \s -> s { errors = (error : (errors s)), continue = False }


addPrototype :: Ident -> Pos -> Type Pos -> [Arg Pos] -> AS ()

addPrototype ident pos type_ args =
    modify $ \s -> s { funMap = M.insert ident (pos, type_, args) (funMap s) }


setRetType :: Type Pos -> AS ()

setRetType type_ =
    modify $ \s -> s { retType = type_, ret = False }


addOuter :: Ident -> Pos -> Type Pos -> AS ()

addOuter ident pos type_ =
    modify $ \s -> s { outVarMap = M.insert ident (pos, type_) (outVarMap s) }


localsToOuter :: [(Ident, (Pos, Type Pos))] -> AS ()

localsToOuter [] =
    return ()

localsToOuter ((ident, entry) : locals) = do
    modify $ \s -> s { outVarMap = M.insert ident entry (outVarMap s) }
    localsToOuter locals


setLocals :: VarMap -> AS ()

setLocals locals =
    modify $ \s -> s { locVarMap = locals }


setOuter :: VarMap -> AS ()

setOuter outer =
    modify $ \s -> s { outVarMap = outer }


cleanLocals :: AS ()

cleanLocals = setLocals M.empty


cleanOuter :: AS ()

cleanOuter = setOuter M.empty


addArgs :: [Arg Pos] -> AS ()

addArgs [] =
    return ()

addArgs (arg:args) = do
    let Arg pos type_ ident = arg
    outer <- gets $ M.lookup ident . outVarMap
    case outer of
        Just (prevPos, _) ->
            msgSameArg ident pos prevPos
        Nothing ->
            addOuter ident pos type_
    addArgs args


msgPos :: String -> Pos -> String

msgPos what (Just (line, char)) =
    what ++ " in line " ++ (show line) ++ " (at pos " ++ (show char) ++ ")\n"


msgRedefined :: String -> Ident -> Pos -> Pos -> String

msgRedefined what (Ident ident) pos prevPos
    = what ++ " " ++ ident ++ " redefined!\n"
    ++ (msgPos "Redefined" pos)
    ++ (msgPos "Previously defined" prevPos)


msgFunDefined :: Ident -> Pos -> Pos -> AS ()

msgFunDefined ident pos prevPos =
    addError $ msgRedefined "Function" ident pos prevPos


msgSameArg :: Ident -> Pos -> Pos -> AS ()

msgSameArg ident pos prevPos =
    addError $ msgRedefined "Argument" ident pos prevPos


msgMainType :: Type Pos -> Pos -> AS ()

msgMainType type_ pos =
    addError $ "Incorrect main type: " ++ (show type_) ++ "!\n"
    ++ (msgPos "Defined" pos)


msgMainArgs :: Pos -> AS ()

msgMainArgs pos =
    addError $ "Function main argument list not empty!\n"
    ++ (msgPos "Defined" pos)


msgNoMain :: AS ()

msgNoMain =
    addError $ "Function main not defined!\n"


msgNoReturn :: Ident -> Pos -> AS ()

msgNoReturn (Ident ident) pos =
    addError $ "Function " ++ ident ++ " has no correct return!\n"
    ++ (msgPos "Defined" pos)


msgVarUndefined :: Ident -> Pos -> AS ()

msgVarUndefined (Ident ident) pos =
    addError $ "Variable " ++ ident ++ " undeclared!\n"
    ++ (msgPos "Used" pos)


msgIncr :: Ident -> Pos -> Pos -> Type Pos -> AS ()

msgIncr (Ident ident) pos prevPos type_ =
    addError $ "Variable " ++ ident ++ " cannot be incremented/decremented\n"
    ++ (msgPos "Used" pos)
    ++ "It is of type " ++ (showType type_) ++ "\n"
    ++ (msgPos "Declared" prevPos)


showType :: Type Pos -> String

showType type_ = case type_ of
    Int pos -> "int"
    Str pos -> "str"
    Bool pos -> "bool"
    Void pos -> "pos"
