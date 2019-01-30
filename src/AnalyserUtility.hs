module AnalyserUtility where

import System.IO
import Control.Monad.State
import qualified Data.Map as M

import AbsLatte
import AbstractTree


putErr = hPutStr stderr
putErrLn = hPutStrLn stderr


type FunMap = M.Map Ident (Pos, Type Pos, [Arg Pos])

type ClassProto = (Pos, FunMap, VarMap, Maybe Ident)

type ClassMap = M.Map Ident ClassProto

type VarMap = M.Map Ident (Pos, Type Pos)

type CheckedMap = M.Map Ident Bool

data AnalysisState = AnalysisState {
    continue :: Bool,
    errors :: [String],
    funMap :: FunMap,
    classMap :: ClassMap,
    checkedClasses :: CheckedMap,
    curClass :: Maybe Ident,
    outVarMap :: VarMap,
    locVarMap :: VarMap,
    retType :: Type Pos,
    curFun :: Ident,
    typeHints :: TypeHints
    } deriving Show

type AS a = State AnalysisState a


startFuns = [
        (Ident "printInt", (defaultPos, Void defaultPos,
            [Arg defaultPos (Int defaultPos) (Ident "toPrint")])),
        (Ident "printString", (defaultPos, Void defaultPos,
            [Arg defaultPos (Str defaultPos) (Ident "toPrint")])),
        (Ident "error", (defaultPos, Void defaultPos, [])),
        (Ident "readInt", (defaultPos, Int defaultPos, [])),
        (Ident "readString", (defaultPos, Str defaultPos, []))
    ]


startState = AnalysisState {
    continue = True,
    errors = [],
    funMap = M.fromList startFuns,
    classMap = M.empty,
    checkedClasses = M.empty,
    curClass = Nothing,
    outVarMap = M.empty,
    locVarMap = M.empty,
    retType = defaultType,
    curFun = Ident "",
    typeHints = M.empty
}


findVar :: Ident -> AS (Maybe (Pos, Type Pos))

findVar ident = do
    loc <- gets $ M.lookup ident . locVarMap
    out <- gets $ M.lookup ident . outVarMap

    case loc of
        Just _ -> return loc
        Nothing -> case out of
            Just _ -> return out
            Nothing -> do
                class_ <- gets curClass
                case class_ of
                    Nothing -> return Nothing
                    Just clIdent -> do
                        Just (_, _, vMap, _) <- gets $ M.lookup clIdent . classMap
                        return $ M.lookup ident vMap


findLoc :: Ident -> AS (Maybe (Pos, Type Pos))

findLoc ident =
    gets $ M.lookup ident . locVarMap


setRetType :: Type Pos -> AS ()

setRetType type_ =
    modify $ \s -> s { retType = type_ }


addError :: String -> AS ()

addError error =
    modify $ \s -> s { errors = (error : (errors s)), continue = False }


addPrototype :: TopDef Pos -> AS ()

addPrototype (FnDef pos type_ ident args _) =
    modify $ \s -> s { funMap = M.insert ident (pos, type_, args) (funMap s) }


addBaseClass :: TopDef Pos -> FunMap -> VarMap -> AS ()

addBaseClass (ClDef pos ident _) fMap vMap =
    modify $ \s -> s { classMap = M.insert ident (pos, fMap, vMap, Nothing) (classMap s) }


addInhClass :: TopDef Pos -> FunMap -> VarMap -> AS ()

addInhClass (ClInher pos this extended _) fMap vMap =
    modify $ \s -> s { classMap = M.insert this (pos, fMap, vMap, Just extended) (classMap s) }


getClassProto :: Ident -> AS (Maybe ClassProto)

getClassProto ident =
    gets $ M.lookup ident . classMap


addUnchecked :: (Ident, ClassProto) -> AS ()

addUnchecked (this, _) =
    modify $ \s -> s { checkedClasses = M.insert this False (checkedClasses s) }


setClass :: Ident -> AS ()

setClass ident =
    modify $ \s -> s { curClass = Just ident }


cleanClass :: AS ()

cleanClass =
    modify $ \s -> s { curClass = Nothing }


addOuter :: Ident -> Pos -> Type Pos -> AS ()

addOuter ident pos type_ =
    modify $ \s -> s { outVarMap = M.insert ident (pos, type_) (outVarMap s) }


addLocal :: Ident -> Pos -> Type Pos -> AS ()

addLocal ident pos type_ =
    modify $ \s -> s { locVarMap = M.insert ident (pos, type_) (locVarMap s) }


localsToOuter :: [(Ident, (Pos, Type Pos))] -> AS ()

localsToOuter [] =
    cleanLocals

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


cmpTypes :: Type Pos -> Type Pos -> Bool

cmpTypes (Int pos1) (Int pos2) = True
cmpTypes (Str pos1) (Str pos2) = True
cmpTypes (Bool pos1) (Bool pos2) = True
cmpTypes (Void pos1) (Void pos2) = True
cmpTypes (Class pos1 ident1) (Class pos2 ident2) = ident1 == ident2
cmpTypes _ _ = False


showType :: Type Pos -> String

showType type_ = case type_ of
    Int pos -> "int"
    Str pos -> "str"
    Bool pos -> "bool"
    Void pos -> "void"
    Class pos (Ident ident) -> "class " ++ ident


defaultPos :: Pos

defaultPos = Just (0,0)


defaultBool :: Type Pos

defaultBool = Bool defaultPos


defaultString :: Type Pos

defaultString = Str defaultPos


defaultInt :: Type Pos

defaultInt = Int defaultPos


defaultType :: Type Pos

defaultType = defaultInt


defaultIdent :: Ident

defaultIdent = Ident "a"

saveState :: AS ()

saveState = do
    state <- get
    addError $ show state


placeHint :: LineChar -> Type_ -> AS ()

placeHint pos type_ =
    modify $ \s -> s { typeHints = M.insert pos type_ (typeHints s) }


singleQuotes :: Ident -> Bool

singleQuotes (Ident ident) = '\'' `elem` ident
