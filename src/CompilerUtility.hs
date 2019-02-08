{-# LANGUAGE ViewPatterns #-}

module CompilerUtility where

import Control.Monad.State

import qualified Data.Map as M
import qualified Data.List as L

import ClassMapConverter
import AbstractTree


type AttrMap = M.Map Ident_ Integer

type MethodMap = M.Map Ident_ Integer

type ClassProto = (AttrMap, MethodMap)

type ClassMap = M.Map Ident_ ClassProto

type VarMap = M.Map String String

data CompilerState = CompilerState {
    code :: [String],
    heap :: [String],
    funProlog :: [String],
    funCode :: [String],
    locVars :: VarMap,
    outVars :: VarMap,
    classes :: ClassMap,
    classTables :: ConvClassMap,
    curClass :: Ident_,
    stackEnd :: Integer,
    maxStack :: Integer,
    labelsCount :: Integer,
    stringsCount :: Integer,
    helpers :: Integer,
    helperWasAddress :: [Bool]
    } deriving Show

type CS a = State CompilerState a


stack = "%ebp"
frame = "%esp"

selfObject = "8(%ebp)"

notRealReg = "notRealReg"


startState :: ConvClassMap -> CompilerState

startState classMap = CompilerState {
    code = [],
    heap = [".section .rodata"],
    funProlog = [],
    funCode = [],
    locVars = M.empty,
    outVars = M.empty,
    classes = M.empty,
    classTables = classMap,
    curClass = Ident_ "",
    stackEnd = 0,
    maxStack = 0,
    labelsCount = 0,
    stringsCount = 0,
    helpers = 0,
    helperWasAddress = []
    }


compileVirtualTables :: ConvClassMap -> [String]

compileVirtualTables classMap =
    M.foldrWithKey compileVirtualTable ["", ".text"] classMap


compileVirtualTable :: Ident_ -> ClassTable -> [String] -> [String]

compileVirtualTable (Ident_ ident) (_, mMap) tables =
    let table = virtualMethods mMap
        label = "__" ++ ident ++ ":"
    in concat [[label], table, tables]


virtualMethods ::[(Ident_, Ident_)] -> [String]

virtualMethods methods =
    foldr virtualMethod [] methods


virtualMethod :: (Ident_, Ident_) -> [String] -> [String]

virtualMethod (class_, method) table =
    let line = "\t.long\t" ++ (mergeIdentsReg class_ method)
    in line : table


setCurClass :: Ident_ -> CS ()

setCurClass ident =
    modify $ \s -> s { curClass = ident }


mergeIdentsReg :: Ident_ -> Ident_ -> String

mergeIdentsReg (Ident_ ident1) (Ident_ ident2) =
    "_" ++ ident2 ++ "__" ++ ident1


mergeIdents :: Ident_ -> Ident_ -> CS Ident_

mergeIdents ident1 ident2 =
    return $ Ident_ $ mergeIdentsReg ident1 ident2


appendSelf :: Ident_ -> [Arg_] -> CS [Arg_]

appendSelf ident args =
    let self = Arg_ (Class_ ident) (Ident_ "self")
    in return $ self : args


addFun :: Ident_ -> CS ()

addFun (Ident_ ident) = do
    let
        globl = case ident of
            '_':_ -> ""
            _ -> "\n.globl " ++ ident
        fun = ident ++ ":"
        pushStack = "\tpushl\t" ++ stack
        saveFrame = "\tmovl\t" ++ frame ++ ", " ++ stack
        prolog = [globl, fun, pushStack, saveFrame]
    modify $ \s -> s { funProlog = reverse prolog }


moveFrame :: CS ()

moveFrame = do
    stackHeight <- gets maxStack
    codeLines <- gets funCode
    let
        reminder = stackHeight `mod` 16
        frameLen = if reminder == 0
            then stackHeight
            else stackHeight - reminder + 16
        line = "\tsubl\t$" ++ (show frameLen) ++ ", " ++ frame
    when (frameLen /= 0) $
        modify $ \s -> s { funProlog = line : funProlog s }


saveFunCode :: CS ()

saveFunCode =
    modify $ \s -> s {
        code = concat [funCode s, funProlog s, code s],
        funProlog = [],
        funCode = []
    }


addArgs :: [Arg_] -> CS ()

addArgs args = foldM_ addArg 8 args


addArg :: Integer -> Arg_ -> CS Integer

addArg pos (Arg_ type_ (Ident_ ident)) = do
    let reg = (show pos) ++ "(" ++ stack ++ ")"
    modify $ \s -> s { outVars = M.insert ident reg (outVars s)}
    return $ pos + 4


clearArgs :: CS ()

clearArgs = modify $ \s -> s {
    outVars = M.insert "_index" "$0" M.empty,
    locVars = M.empty
    }


moveVars :: CS (VarMap, VarMap)

moveVars = do
    loc <- gets locVars
    out <- gets outVars
    forM_ (M.toList loc) addOutVar
    clearLoc
    return (loc, out)


addOutVar :: (String, String) -> CS ()

addOutVar (name, pos) =
    modify $ \s -> s { outVars = M.insert name pos (outVars s) }


setVars :: (VarMap, VarMap) -> CS ()

setVars (loc, out) =
    modify $ \s -> s { locVars = loc, outVars = out }


clearLoc :: CS ()

clearLoc =
    modify $ \s -> s { locVars = M.empty }


getVar :: String -> CS String

getVar ident =
    case ident of
        "_index" -> do
            out <- gets $ M.lookup ident . outVars
            case out of
                Just "$0" -> do
                    pos <- addStack "_index"
                    return pos
                Just out -> return out
        _ -> do
            loc <- gets $ M.lookup ident . locVars
            case loc of
                Just pos -> return pos
                Nothing -> do
                    out <- gets $ M.lookup ident . outVars
                    case out of
                        Just pos -> return pos
                        Nothing -> do
                            class_ <- gets curClass
                            getAttribute class_ selfObject (Ident_ ident) "%eax"


addLocalVar :: String -> String -> CS ()

addLocalVar ident pos =
    modify $ \s -> s { locVars = M.insert ident pos (locVars s) }


addOuterVar :: String -> String -> CS ()

addOuterVar ident pos =
    modify $ \s -> s { outVars = M.insert ident pos (outVars s) }


addHelper :: String -> CS String

addHelper pos =
    let pos_ = extractReg pos
    in if isRegister pos_
        then do
            count <- gets helpers
            stackPos <- addStack $ "_helper" ++ (show count)
            transferValues pos_ stackPos
            modify $ \s -> s {
                helpers = count + 1,
                helperWasAddress = (pos /= pos_) : (helperWasAddress s)
            }
            return stackPos
        else
            return pos


getHelperStrict :: String -> String -> CS String

getHelperStrict helper aux = do
    strictMovl helper aux

    thisHelperWasAddress <- gets $ head . helperWasAddress
    modify $ \s -> s {
        stackEnd = stackEnd s - 4,
        helperWasAddress = tail (helperWasAddress s)
    }

    if thisHelperWasAddress
        then return $ "(" ++ aux ++ ")"
        else return aux


getHelper :: String -> String -> String -> String -> CS String

getHelper helper res2 aux1 aux2 = do
    let
        taken = extractReg res2
        aux = if taken == aux1
            then aux2
            else aux1
    getHelperStrict helper aux


getClassTable :: Ident_ -> CS ClassTable

getClassTable ident = do
    Just table <- gets $ M.lookup ident . classTables
    return table


findAttrOffset :: Ident_ -> Ident_ -> CS Integer

findAttrOffset class_ (Ident_ attr) = do
    (vMap, _) <- getClassTable class_
    let offset = findAttr vMap 0 (-1) attr
    return $ 4 * (offset + 1)


findAttr :: VMap -> Integer -> Integer -> String -> Integer

findAttr [] which max_ attr = max_

findAttr ((_, (Ident_ clAttr)):vMap) which max_ attr =
    if clAttr == attr
        then findAttr vMap (which + 1) which attr
        else findAttr vMap (which + 1) max_ attr


getAttribute :: Ident_ -> String -> Ident_ -> String -> CS String

getAttribute class_ object attr res = do
    offset <- findAttrOffset class_ attr
    let
        attrReg =
            case res of
                "%eax" -> "%ecx"
                _ -> "%eax"

    attrReg <- tryMovl object attrReg
    let attrLoc = (show offset) ++ "(" ++ attrReg ++ ")"
    return attrLoc


findMethodOffset :: Ident_ -> Ident_ -> CS Integer

findMethodOffset class_ (Ident_ method) = do
    (_, mMap) <- getClassTable class_
    let offset = findMethod mMap 0 method
    return $ 4 * offset


findMethod :: MMap -> Integer -> String -> Integer

findMethod ((_, (Ident_ clMethod)):mMap) which method =
    if clMethod == method
        then which
        else findMethod mMap (which + 1) method


getMethod :: Ident_ -> String -> Ident_ -> CS String

getMethod class_ object method = do
    offset <- findMethodOffset class_ method
    objectAddress <- tryMovl object "%eax"
    tableAddress <- tryMovl "(%eax)" "%eax"
    let funLoc = "*" ++ (show offset) ++ "(%eax)"
    return funLoc


restoreEsp :: [Expr_] -> CS ()

restoreEsp exprs =
    let
        len = length exprs
        lenConstant = "$" ++ (show (4 * len))
    in when (len /= 0) $ emitDouble "addl" lenConstant frame


restoreEspLen :: Integer -> CS ()

restoreEspLen argsCount =
    let
        lenConstant = "$" ++ (show (4 * argsCount))
    in when (argsCount /= 0) $ emitDouble "addl" lenConstant frame


addLines :: [String] -> CS ()

addLines toAdd =
    modify $ \s -> s { funCode = concat [reverse toAdd, (funCode s)] }


tryMovl :: String -> String -> CS String

tryMovl pos res =
    case pos of
        '%':_ -> return pos
        _ -> do
            emitDouble "movl" pos res
            return res


strictMovl :: String -> String -> CS String

strictMovl pos res =
    if pos == res
        then return res
        else do
            emitDouble "movl" pos res
            return res


chooseReg :: String -> String -> String -> CS (String, String)

chooseReg pos1 pos2 def =
    case pos1 of
        '%':_ -> return (pos1, pos2)
        _ -> case pos2 of
            '%':_ -> return (pos2, pos1)
            _ -> do
                res <- tryMovl pos1 def
                return (res, pos2)


setDivision :: String -> String -> String -> String -> String -> CS String

setDivision pos1 pos2 dest1 dest2 aux =
    if dest1 == pos2
        then do
            if dest2 == pos1
                then do
                    strictMovl pos2 aux
                    strictMovl pos1 dest1
                    strictMovl aux dest2
                else do
                    strictMovl pos2 dest2
                    strictMovl pos1 dest1
            return dest2
        else do
            strictMovl pos1 dest1
            if isConstant pos2
                then do
                    strictMovl pos2 dest2
                    return dest2
                else
                    return pos2


setSubtract :: String -> String -> String -> String -> CS String

setSubtract pos1 pos2 dest1 dest2 =
    if dest1 == pos2
        then do
            res <- tryMovl pos1 dest2
            return res
        else do
            res <- tryMovl pos1 dest1
            return res


addHeapLine :: String -> CS ()

addHeapLine line =
    modify $ \s -> s { heap = line : (heap s) }


addToHeap :: String -> CS String

addToHeap str = do
    label <- nextString
    addHeapLine $ label ++ ":"
    let
        strRep = if str == ""
            then "\"\""
            else str
    addHeapLine $ "\t.string " ++ strRep
    return label


addLabel :: String -> CS ()

addLabel label = addLines [label ++ ":"]


nextLabel :: CS String

nextLabel = do
    count <- gets labelsCount
    modify $ \s -> s { labelsCount = labelsCount s + 1 }
    return $ ".LF" ++ (show count)


nextString :: CS String

nextString = do
    count <- gets stringsCount
    modify $ \s -> s { stringsCount = count + 1 }
    return $ ".LC" ++ (show count)


addStack :: String -> CS String

addStack ident = do
    modify $ \s -> s { stackEnd = (stackEnd s + 4) }
    modify $ \s -> s { maxStack = max (stackEnd s) (maxStack s) }
    offset <- gets stackEnd
    let pos = "-" ++ (show offset) ++ "(%ebp)"
    case ident of
        "_index" -> addOuterVar ident pos
        _ -> addLocalVar ident pos
    return pos


zeroStack :: CS ()

zeroStack =
    modify $ \s -> s { stackEnd = 0, maxStack = 0 }


emitInstr :: String -> CS ()

emitInstr instr =
    let line = "\t" ++ instr
    in addLines [line]


padded :: Int -> String -> String

padded len str =
    let
        strLen = length str
        padLen = if strLen > len
            then 0
            else len - strLen
        spaces = ' ':spaces
        padding = take padLen spaces
    in str ++ padding


emitSingle :: String -> String -> CS ()

emitSingle instr arg =
    let line = "\t" ++ (padded 4 instr) ++ "\t" ++ arg
    in addLines [line]


emitDouble :: String -> String -> String -> CS ()

emitDouble instr arg1 arg2 =
    let line = "\t" ++ (padded 4 instr) ++ "\t" ++ arg1 ++ ", " ++ arg2
    in addLines [line]


transferValues :: String -> String -> CS ()

transferValues res var =
    if res == var
        then return ()
        else case (res, var) of
            ('%':_, _) ->
                emitDouble "movl" res var
            ('$':_, _) ->
                emitDouble "movl" res var
            (_, _) -> do
                let
                    extrVar = extractReg var
                    tmp = case extrVar of
                        "%eax" -> "%ecx"
                        _ -> "%eax"
                emitDouble "movl" res tmp
                emitDouble "movl" tmp var


callCalloc :: String -> CS ()

callCalloc res = do
    emitSingle "pushl" res
    emitSingle "pushl" "$4"
    emitSingle "call" "calloc"
    restoreEspLen 2


getArrayElem :: String -> String -> String -> CS String

getArrayElem ident res_ taken_ = do
    arrarAddress <- getVar ident

    let
        res = extractReg res_
        taken = extractReg taken_
        aux = if (res /= "%eax") && (taken /= "%eax")
                then "%eax"
                else if (res /= "%ecx") && (taken /= "%ecx")
                    then "%ecx"
                    else "%edx"
    emitDouble "movl" arrarAddress aux
    emitDouble "leal" ("4(" ++ aux ++ ", " ++ res ++ ", 4)") aux
    return $ "(" ++ aux ++ ")"


extractReg :: String -> String

extractReg pos =
    if length pos > 5
        then take 4 $ takeLast 5 pos
        else pos


takeLast :: Int -> [a] -> [a]
takeLast n l = go (drop n l) l
  where
    go [] r = r
    go (_:xs) (_:ys) = go xs ys


checkMultiple :: String -> String -> CS Bool

checkMultiple ident pos = do
    let pred (k, v) = (k /= ident) && (pos == v)
    loc <- gets locVars
    out <- gets outVars
    let
        filteredLoc = filter pred $ M.toList loc
        filteredOut = filter pred $ M.toList out
    return $ (filteredLoc == []) && (filteredOut == [])


lastLine :: CS String

lastLine = do
    code <- gets funCode
    case code of
        _:_ -> return $ head code
        _ -> return ""


lastLineRet :: CS Bool

lastLineRet = do
    line <- lastLine
    return $ "\tret" `L.isPrefixOf` line


checkEmptyLabel :: CS ()

checkEmptyLabel = do
    line <- lastLine
    if (".L" `L.isPrefixOf` line)
        then modify $ \s -> s { funCode = tail (funCode s) }
        else return ()


isRegister :: String -> Bool

isRegister pos = case pos of
    "%eax" -> True
    "%ecx" -> True
    "%edx" -> True
    _ -> False


isConstant :: String -> Bool

isConstant pos = case pos of
    '$':_ -> True
    _ -> False


isMemory :: String -> Bool

isMemory pos = not $ (isRegister pos) || (isConstant pos)
