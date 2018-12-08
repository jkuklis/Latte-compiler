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
    ret :: Bool,
    curFun :: Ident
    } deriving Show

type AS a = State AnalysisState a

startState = AnalysisState {
    continue = True,
    errors = [],
    funMap = M.empty,
    outVarMap = M.empty,
    locVarMap = M.empty,
    retType = defaultType,
    ret = True,
    curFun = Ident ""
}


findVar :: Ident -> AS (Maybe (Pos, Type Pos))

findVar ident = do
    loc <- gets $ M.lookup ident . locVarMap
    out <- gets $ M.lookup ident . outVarMap
    case loc of
        Just _ -> return loc
        Nothing -> return out


findLoc :: Ident -> AS (Maybe (Pos, Type Pos))

findLoc ident =
    gets $ M.lookup ident . locVarMap


setRetType :: Type Pos -> AS ()

setRetType type_ =
    modify $ \s -> s { retType = type_, ret = False }


markReturn :: AS ()

markReturn =
    modify $ \s -> s { ret = True }


addError :: String -> AS ()

addError error =
    modify $ \s -> s { errors = (error : (errors s)), continue = False }


addPrototype :: Ident -> Pos -> Type Pos -> [Arg Pos] -> AS ()

addPrototype ident pos type_ args =
    modify $ \s -> s { funMap = M.insert ident (pos, type_, args) (funMap s) }


addOuter :: Ident -> Pos -> Type Pos -> AS ()

addOuter ident pos type_ =
    modify $ \s -> s { outVarMap = M.insert ident (pos, type_) (outVarMap s) }


addLocal :: Ident -> Pos -> Type Pos -> AS ()

addLocal ident pos type_ =
    modify $ \s -> s { locVarMap = M.insert ident (pos, type_) (locVarMap s) }


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


cleanVars :: AS ()

cleanVars = do
    cleanOuter
    cleanLocals


cleanLocals :: AS ()

cleanLocals = setLocals M.empty


cleanOuter :: AS ()

cleanOuter = setOuter M.empty


setCur :: Ident -> AS ()

setCur ident =
    modify $ \s -> s { curFun = ident }


posInfo :: String -> Pos -> String

posInfo what (Just (line, char)) =
    what ++ " in line " ++ (show line) ++ " (at pos " ++ (show char) ++ ")\n"


msgRedefined :: String -> Ident -> Pos -> Pos -> AS ()

msgRedefined what (Ident ident) pos prevPos =
    addError $ what ++ " " ++ ident ++ " redefined!\n"
    ++ (posInfo "Redefined" pos)
    ++ (posInfo "Previously defined" prevPos)


msgFunDefined :: Ident -> Pos -> Pos -> AS ()

msgFunDefined ident pos prevPos =
    msgRedefined "Function" ident pos prevPos


msgSameArg :: Ident -> Pos -> Pos -> AS ()

msgSameArg ident pos prevPos =
    msgRedefined "Argument" ident pos prevPos


msgMainType :: Type Pos -> Pos -> AS ()

msgMainType type_ pos =
    addError $ "Incorrect main type: " ++ (show type_) ++ "!\n"
    ++ (posInfo "Defined" pos)


msgMainArgs :: Pos -> AS ()

msgMainArgs pos =
    addError $ "Function main argument list not empty!\n"
    ++ (posInfo "Defined" pos)


msgNoMain :: AS ()

msgNoMain =
    addError $ "Function main not defined!\n"


msgNoReturn :: Ident -> Pos -> AS ()

msgNoReturn (Ident ident) pos =
    addError $ "Function " ++ ident ++ " has no correct return!\n"
    ++ (posInfo "Defined" pos)


msgVarUndefined :: Ident -> Pos -> AS ()

msgVarUndefined (Ident ident) pos =
    addError $ "Variable " ++ ident ++ " undeclared!\n"
    ++ (posInfo "Used" pos)


msgIncr :: Ident -> Pos -> Pos -> Type Pos -> AS ()

msgIncr (Ident ident) pos prevPos type_ =
    addError $ "Variable " ++ ident ++ " cannot be incremented/decremented\n"
    ++ (posInfo "Used" pos)
    ++ "It is of type " ++ (showType type_) ++ "\n"
    ++ (posInfo "Declared" prevPos)


msgAssign :: Ident -> Pos -> Type Pos -> Type Pos -> Pos -> AS ()

msgAssign (Ident ident) pos eType type_ prevPos =
    addError $ "Incorrect type in " ++ ident ++ " assignment!\n"
    ++ (posInfo "Assignment" pos)
    ++ "Variable type: " ++ (showType type_)
    ++ ", expression type: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Defined" prevPos)


msgReturn :: Pos -> Type Pos -> AS ()

msgReturn pos eType = do
    Ident ident <- gets curFun
    rType <- gets retType
    addError $ "Incorrect type in " ++ ident ++ " return!\n"
        ++ "Function return type: " ++ (showType rType)
        ++ ", expression type: " ++ (showType eType) ++ "\n"
        ++ (posInfo "Return" pos)


msgCond :: Pos -> Type Pos -> AS ()

msgCond pos eType =
    addError $ (showType eType) ++ " instead of a boolean in condition!\n"
    ++ (posInfo "Used" pos)


msgExpDecl :: Ident -> Pos -> Type Pos -> Type Pos -> AS ()

msgExpDecl (Ident ident) pos dType eType =
    addError $ "Incorrect expression type in " ++ ident ++ " declaration!\n"
    ++ "Expected: " ++ (showType dType)
    ++ ", got: " ++ (showType eType) ++ "\n"
    ++ (posInfo "Declared" pos)


msgVarDeclared :: Ident -> Pos -> Pos -> AS ()

msgVarDeclared ident pos prevPos =
    msgRedefined "Variable" ident pos prevPos


cmpTypes :: Type Pos -> Type Pos -> Bool

cmpTypes (Int pos1) (Int pos2) = True
cmpTypes (Str pos1) (Str pos2) = True
cmpTypes (Bool pos1) (Bool pos2) = True
cmpTypes _ _ = False


showType :: Type Pos -> String

showType type_ = case type_ of
    Int pos -> "int"
    Str pos -> "str"
    Bool pos -> "bool"


defaultPos :: Pos

defaultPos = Just $ (0,0)


defaultType :: Type Pos

defaultType = Int defaultPos


saveState :: AS ()

saveState = do
    state <- get
    addError $ show state
