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
    stackEnd :: Integer,
    maxStack :: Integer,
    labelsCount :: Integer,
    stringsCount :: Integer,
    helpers :: Integer
    } deriving Show

type CS a = State CompilerState a


stack = "%ebp"
frame = "%esp"


startState :: ConvClassMap -> CompilerState

startState classMap = CompilerState {
    code = [".text"],
    heap = [".section .rodata"],
    funProlog = [],
    funCode = [],
    locVars = M.empty,
    outVars = M.empty,
    classes = M.empty,
    classTables = classMap,
    stackEnd = 0,
    maxStack = 0,
    labelsCount = 0,
    stringsCount = 0,
    helpers = 0
    }


addFun :: Ident_ -> CS ()

addFun (Ident_ ident) = do
    let
        globl = "\n.globl " ++ ident
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
    outVars = M.empty,
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

getVar ident = do
    loc <- gets $ M.lookup ident . locVars
    case loc of
        Just pos -> return pos
        Nothing -> do
            out <- gets $ M.lookup ident . outVars
            case out of
                Just pos -> return pos
                Nothing -> do
                    pos <- addStack ident
                    return pos


addLocalVar :: String -> String -> CS ()

addLocalVar ident pos =
    modify $ \s -> s { locVars = M.insert ident pos (locVars s) }


addHelper :: String -> CS String

addHelper pos =
    if isRegister pos
        then do
            count <- gets helpers
            stackPos <- addStack $ "_helper" ++ (show count)
            strictMovl pos stackPos
            modify $ \s -> s { helpers = count + 1 }
            return stackPos
        else
            return pos


getHelper :: String -> String -> String -> String -> CS String

getHelper helper res2 aux1 aux2 = do
    modify $ \s -> s { stackEnd = stackEnd s - 4 }
    if res2 == aux1
        then do
            strictMovl helper aux2
            return aux2
        else do
            strictMovl helper aux1
            return aux1


getClassTable :: Ident_ -> CS ClassTable

getClassTable ident = do
    Just table <- gets $ M.lookup ident . classTables
    return table


findOffset :: Ident_ -> Ident_ -> CS Integer

findOffset class_ (Ident_ attr) = do
    (vMap, _) <- getClassTable class_
    return $ 4 * ((find vMap 0 (-1) attr) + 1)


find :: VMap -> Integer -> Integer -> String -> Integer

find [] which max_ attr = max_

find ((_, (Ident_ clAttr)):vMap) which max_ attr =
    if clAttr == attr
        then find vMap (which + 1) which attr
        else find vMap (which + 1) max_ attr


getAttribute :: Ident_ -> String -> Ident_ -> String -> CS String

getAttribute class_ object attr res = do
    offset <- findOffset class_ attr
    let
        attrReg =
            case res of
                "%eax" -> "%ecx"
                _ -> "%eax"

    attrReg <- tryMovl object attrReg
    let attrLoc = (show offset) ++ "(" ++ attrReg ++ ")"
    return attrLoc


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
    addLocalVar ident pos
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
                    tmp = if length var >= 5
                        then case takeLast 6 var of
                            "(%eax)" -> "%ecx"
                            _ -> "%eax"
                        else case var of
                            "%eax" -> "%ecx"
                            _ -> "%eax"
                emitDouble "movl" res tmp
                emitDouble "movl" tmp var


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
    '%':_ -> True
    _ -> False


isConstant :: String -> Bool

isConstant pos = case pos of
    '$':_ -> True
    _ -> False


isMemory :: String -> Bool

isMemory pos = not $ (isRegister pos) || (isConstant pos)


mergeIdents :: Ident_ -> Ident_ -> CS Ident_

mergeIdents (Ident_ ident1) (Ident_ ident2) =
    return $ Ident_ $ "_" ++ ident1 ++ "_" ++ ident2
