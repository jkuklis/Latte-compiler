{-# LANGUAGE ViewPatterns #-}

module CompilerUtility where

import Control.Monad.State

import qualified Data.Map as M

import AbstractTree


type VarMap = M.Map String String

data CompilerState = CompilerState {
    code :: [String],
    heap :: [String],
    locVars :: VarMap,
    outVars :: VarMap,
    stackEnd :: Integer,
    maxStack :: Integer,
    labelsCount :: Integer,
    stringsCount :: Integer,
    helpers :: Integer
    } deriving Show

type CS a = State CompilerState a


stack = "%ebp"
frame = "%esp"

startState = CompilerState {
    code = [".text"],
    heap = [".section .rodata"],
    locVars = M.empty,
    outVars = M.empty,
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
        stackOff = "..stack_holder"

    addLines [globl, fun]
    emitSingle "pushl" stack
    emitDouble "movl" frame stack
    modify $ \s -> s { code = stackOff : (code s) }


moveFrame :: CS ()

moveFrame = do
    stackHeight <- gets maxStack
    codeLines <- gets code
    let
        reminder = stackHeight `mod` 16
        frameLen = if reminder == 0
            then stackHeight
            else stackHeight - reminder + 16
        line = if frameLen == 0
            then ""
            else "\tsubl\t$" ++ (show frameLen) ++ ", " ++ frame
        repLines = map (\l -> if l == "..stack_holder" then line else l) codeLines
    modify $ \s -> s { code = repLines }


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

addHelper pos = do
    count <- gets helpers
    stackPos <- addStack $ "_helper" ++ (show count)
    let
        intermediate = case pos of
            '%':_ -> pos
            _ -> "%eax"
    case pos of
        '%':_ -> return ()
        _ -> emitDouble "movl" pos intermediate
    emitDouble "movl" intermediate stackPos
    modify $ \s -> s { helpers = count + 1 }
    return stackPos


getHelper :: String -> String -> CS String

getHelper helper reg = do
    let
        res = case reg of
            "%eax" -> "%ecx"
            _ -> "%eax"
    emitDouble "movl" helper res
    modify $ \s -> s { stackEnd = stackEnd s - 4 }
    return res


addLines :: [String] -> CS ()

addLines toAdd =
    modify $ \s -> s { code = concat [reverse toAdd, (code s)] }


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


checkMultiple :: String -> String -> CS Bool

checkMultiple ident pos = do
    let pred (k, v) = (k /= ident) && (pos == v)
    loc <- gets locVars
    out <- gets outVars
    let
        filteredLoc = filter pred $ M.toList loc
        filteredOut = filter pred $ M.toList out
    return $ (filteredLoc == []) && (filteredOut == [])
