module Analyser where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte
import ParLatte
import ErrM

import AbstractTree

import AnalyserUtility
import AnalyserErrors
import AnalyserClasses
import AnalyserExpressions


analyse :: String -> IO (Bool, TypeHints, ClassMap)

analyse input =
    let tokens = myLexer input in
        case pProgram tokens of
            Bad error -> do
                putErrLn "ERROR\n"
                putErrLn "Failure to parse program!"
                putErrLn $ error ++ "\n"
                return (False, M.empty, M.empty)

            Ok (Program _ defs) -> do
                -- putErrLn $ show defs
                let statePrototypes = execState (getPrototypes defs) startState
                let state = execState (checkDefinitions defs) statePrototypes
                if continue state
                    then
                        putErrLn "OK\n"
                    else do
                        putErrLn "ERROR\n"
                        putErr $ unlines $ reverse $ errors state
                return (continue state, typeHints state, classMap state)


getPrototypes :: [TopDef Pos] -> AS ()

getPrototypes [] = do
    main <- gets $ M.lookup (Ident "main") . funMap
    when (main == Nothing) $ msgNoMain
    checkInheritance


getPrototypes (def:defs) = do
    case def of
        FnDef _ _ _ _ _ -> getFunProto def
        ClDef _ _ _ -> getBaseClassProto def
        ClInher _ _ _ _ -> getInhClassProto def

    getPrototypes defs


checkInheritance :: AS ()

checkInheritance = do
    classes <- gets classMap
    forM_ (M.toList classes) addUnchecked
    forM_ (M.toList classes) (checkInherits [] classes)


checkInherits :: [Ident] -> ClassMap -> (Ident, ClassProto) -> AS ()

checkInherits previous classes (this, (pos, _, _, extended)) = do
    Just checked <- gets $ M.lookup this . checkedClasses
    if not checked
        then return ()
        else do
            modify $ \s -> s { checkedClasses = M.insert this True (checkedClasses s) }
            case extended of
                Nothing -> return ()
                Just ext -> do
                    let extProto = M.lookup ext classes
                    if extProto == Nothing
                        then msgNoBaseClass pos this ext
                        else if this == ext
                            then msgExtendSelf pos this
                            else if elem this previous
                                then msgCyclicDep pos this ext
                                else
                                    let Just proto = extProto
                                    in checkInherits (this : previous) classes (ext, proto)


getFunProto :: TopDef Pos -> AS ()

getFunProto f@(FnDef pos type_ ident args block) = do
    fun <- gets $ M.lookup ident . funMap
    case fun of
        Just (prevPos, _, _) ->
            msgFunDefined ident pos prevPos
        Nothing ->
            if ident == Ident "main"
                then do
                    when (type_ /= Int pos) $ msgMainType type_ pos
                    when (args /= []) $ msgMainArgs pos
                    when (type_ == Int pos && args == []) $ addPrototype f
                else do
                    when (singleQuotes ident) $ msgQuote pos ident
                    addPrototype f


getBaseClassProto :: TopDef Pos -> AS ()

getBaseClassProto c@(ClDef pos ident block) = do
    class_ <- gets $ M.lookup ident . classMap
    case class_ of
        Just (prevPos, _, _, _) ->
            msgClassDefined ident pos prevPos
        Nothing -> do
            (funMap, varMap) <- examineClassBlock block
            addBaseClass c funMap varMap


getInhClassProto :: TopDef Pos -> AS ()

getInhClassProto c@(ClInher pos this extended block) = do
    class_ <- gets $ M.lookup this . classMap
    case class_ of
        Just (prevPos, _, _, _) ->
            msgClassDefined this pos prevPos
        Nothing -> do
            (funMap, varMap) <- examineClassBlock block
            addInhClass c funMap varMap


examineClassBlock :: ClassBlock Pos -> AS (FunMap, VarMap)

examineClassBlock (ClBlock pos classMembers) =
    let
        fMap = M.empty
        vMap = M.empty
    in examineMembers classMembers fMap vMap


examineMembers :: [ClMember Pos] -> FunMap -> VarMap -> AS (FunMap, VarMap)

examineMembers [] fMap vMap = return (fMap, vMap)

examineMembers ((ClAttr pos type_ ident):members) fMap vMap =
    let lookedUp = M.lookup ident vMap
    in case lookedUp of
        Just (prevPos, _) -> do
            msgPrevAttr ident pos prevPos
            examineMembers members fMap vMap
        Nothing ->
            let newVMap = M.insert ident (pos, type_) vMap
            in examineMembers members fMap newVMap


examineMembers ((ClFun pos type_ ident args _):members) fMap vMap =
    let lookedUp = M.lookup ident fMap
    in case lookedUp of
        Just (prevPos, _, _) -> do
            msgPrevMet ident pos prevPos
            examineMembers members fMap vMap
        Nothing ->
            let newFMap = M.insert ident (pos, type_, args) fMap
            in examineMembers members newFMap vMap


checkDefinitions :: [TopDef Pos] -> AS ()

checkDefinitions [] =
    return ()

checkDefinitions (def:defs) = do
    case def of
        FnDef pos type_ ident args (Block bPos stmts) -> do
            case type_ of
                Class cPos classIdent -> do
                    class_ <- getClassProto classIdent
                    when (class_ == Nothing) $ msgClassUndefined classIdent cPos
                _ -> return ()
            setRetType type_
            setCur ident
            cleanVars
            addArgs args
            returned <- checkStmts stmts
            case type_ of
                Void tPos ->
                    return ()
                _ -> do
                    when (not returned) $ msgNoReturn ident pos

        ClDef pos ident (ClBlock bPos members) -> do
            setClass ident
            checkClassMembers members
            cleanClass

        ClInher pos ident extended (ClBlock bPos members) -> do
            setClass ident
            checkClassMembers members
            cleanClass

    checkDefinitions defs


checkClassMembers :: [ClMember Pos] -> AS ()

checkClassMembers [] =
    return ()

checkClassMembers ((ClAttr pos type_ ident):members) =
    checkClassMembers members

checkClassMembers ((ClFun pos type_ ident args block):members) = do
    checkDefinitions [FnDef pos type_ ident args block]
    checkClassMembers members


addArgs :: [Arg Pos] -> AS ()

addArgs [] =
    return ()


addArgs ((Arg pos type_ ident):args) = do
    when (singleQuotes ident) $ msgQuote pos ident

    case type_ of
        Void _ ->
            msgVoidArg pos ident
        Class cPos classIdent -> do
            class_ <- getClassProto classIdent
            when (class_ == Nothing) $ msgClassUndefined classIdent cPos
        _ ->
            return ()
    outer <- gets $ M.lookup ident . outVarMap
    case outer of
        Just (prevPos, _) ->
            msgSameArg ident pos prevPos
        Nothing ->
            addOuter ident pos type_
    addArgs args


checkStmts :: [Stmt Pos] -> AS Bool

checkStmts [] =
    return False

checkStmts (st:sts) = do
    returnedSt <- checkStmt st
    returnedStmts <- checkStmts sts
    return $ returnedSt || returnedStmts


checkStmt :: Stmt Pos -> AS Bool

checkStmt st = case st of
    Empty pos ->
        return False

    BStmt pos (Block bPos stmts) -> do
        locals <- gets locVarMap
        outer <- gets outVarMap
        localsToOuter $ M.toList locals
        returned <- checkStmts stmts
        setLocals locals
        setOuter outer
        return returned

    Decl pos dType items -> do
        case dType of
            Void _ ->
                msgVoidVar pos

            Class _ ident -> do
                class_ <- getClassProto ident
                when (class_ == Nothing) $ msgClassUndefined ident pos
                checkDecl dType items

            _ ->
                checkDecl dType items

        return False

    Ass pos ident expr -> do
        eType <- checkExpr expr
        var <- findVar pos ident
        case var of
            Just (prevPos, vType) -> do
                checkTypes eType vType $ msgAssign ident pos vType prevPos
            Nothing ->
                msgVarUndeclared ident pos
        return False

    AttrAss pos object attr expr -> do
        eType <- checkExpr expr
        aType <- getObjectAttrType pos object attr

        case aType of
            Nothing -> return ()
            Just aType -> do
                checkTypes eType aType $ msgAttrType pos object attr aType

        return False

    SelfAtAss pos attr expr -> do
        eType <- checkExpr expr

        class_ <- gets curClass
        case class_ of
            Nothing -> msgSelfAttr pos attr
            _ -> do
                aType <- getClassAttrType pos class_ attr
                case aType of
                    Nothing -> return ()
                    Just aType -> do
                        checkTypes eType aType $
                            msgAttrType pos (Ident "self") attr aType


        return False

    Incr pos ident -> do
        var <- findVar pos ident
        checkIncr pos ident var

    Decr pos ident -> checkStmt $ Incr pos ident

    SelfIncr pos ident -> do
        var <- findAttr pos ident
        checkIncr pos ident var

    SelfDecr pos ident -> checkStmt $ SelfIncr pos ident

    Ret pos expr -> do
        eType <- checkExpr expr
        rType <- gets retType
        case rType of
            Void fPos -> do
                msgNotVoidReturn pos
                return False
            _ -> case eType of
                Nothing ->
                    return False
                Just (eType) -> do
                    classes <- gets classMap
                    if cmpTypes rType eType classes
                        then return True
                        else do
                            msgReturn pos eType
                            return False

    VRet pos -> do
        rType <- gets retType
        case rType of
            Void fPos ->
                return True
            _ -> do
                msgVoidReturn pos
                return False

    Cond pos expr stmt -> checkStmt $ CondElse pos expr stmt (Empty pos)

    CondElse pos expr stmt1 stmt2 -> do
        eType <- checkExpr expr
        checkTypes eType defaultBool $ msgCond pos
        ret1 <- checkStmt $ BStmt pos $ Block pos [stmt1]
        ret2 <- checkStmt $ BStmt pos $ Block pos [stmt2]
        constCond <- constantBool expr
        case constCond of
            Just True ->
                return ret1
            Just False ->
                return ret2
            Nothing ->
                return $ ret1 && ret2

    While pos expr stmt -> do
        ret <- checkStmt $ Cond pos expr stmt
        constCond <- constantBool expr
        case constCond of
            Just True ->
                return True
            _ -> return ret

    SExp pos expr -> do
        checkExpr expr
        return False


checkIncr :: Pos -> Ident -> Maybe VarProto -> AS Bool

checkIncr pos ident var = do
    case var of
        Just (tPos, type_) ->
            case type_ of
                Int tPos ->
                    return ()
                _ ->
                    msgIncr ident pos tPos type_
        Nothing -> msgVarUndeclared ident pos
    return False


checkDecl :: Type Pos -> [Item Pos] -> AS ()

checkDecl _ [] =
    return ()

checkDecl dType (item:items) = case item of
    Init pos ident expr -> do
        eType <- checkExpr expr
        checkTypes eType dType $ msgExpDecl ident pos dType
        checkDecl dType $ (NoInit pos ident) : items

    NoInit pos ident -> do
        when (singleQuotes ident) $ msgQuote pos ident
        var <- findLoc ident
        case var of
            Just (vPos, vType) ->
                msgVarDeclared ident pos vPos
            Nothing ->
                addLocal ident pos dType
        checkDecl dType items
