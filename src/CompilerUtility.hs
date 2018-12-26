{-# LANGUAGE ViewPatterns #-}

module CompilerUtility where

import Control.Monad.State

import qualified Data.Map as M
import qualified Data.List as L

import AbstractTree


type RegMap = M.Map String (Maybe String)

type VarMap = M.Map String String

data CompilerState = CompilerState {
    code :: [String],
    regs :: RegMap,
    locVars :: VarMap,
    outVars :: VarMap,
    stackEnd :: Integer
    } deriving Show

type CS a = State CompilerState a

-- registers name pattern = definition
--     ["rax", "rcx", "rdx",
--         "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11"]

registers = ["eax", "ecx", "edx"] -- , "ebx", "esi", "edi]
registersMap = foldr (\reg m -> M.insert reg Nothing m) M.empty registers
stack = "%ebp"
frame = "%esp"

startState = CompilerState {
    code = [],
    regs = registersMap,
    locVars = M.empty,
    outVars = M.empty,
    stackEnd = 0
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
    stackHeight <- gets stackEnd
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
    let reg = (show pos) ++ "(%" ++ stack ++ ")"
    modify $ \s -> s { outVars = M.insert ident reg (outVars s)}
    return $ pos + 4


clearArgs :: CS ()

clearArgs = modify $ \s -> s {
    outVars = M.empty,
    locVars = M.empty,
    regs = registersMap }


addBlock :: Block_ -> CS ()

addBlock (Block_ stmts) = addStmts stmts


addStmts :: [Stmt_] -> CS ()

addStmts stmts = forM_ stmts addStmt


addStmt :: Stmt_ -> CS ()

addStmt stmt = do
    case stmt of
        Empty_ -> return ()

        BStmt_ block -> do
            (loc, out) <- moveVars
            addBlock block
            setVars (loc, out)

        Decl_ type_ items ->
            addDecls type_ items

        Ass_ ident expr ->
            addAss ident expr

        Incr_ ident ->
            incr ident 1

        Decr_ ident ->
            incr ident (-1)

        Ret_ expr ->
            exprRet expr

        VRet_ ->
            voidRet

        _ -> return ()


checkVoidRet :: Type_ -> CS ()

checkVoidRet type_ = do
    (line:lines) <- gets code
    if (type_ == Void_) && (not ("\tret" `L.isPrefixOf` line))
        then voidRet
        else return ()


voidRet :: CS ()

voidRet = do
    emitInstr "leave"
    emitInstr "ret"


exprRet :: Expr_ -> CS ()

exprRet expr = do
    pos <- addExpr expr
    if pos == "%eax"
        then return ()
        else emitDouble "movl" pos "%eax"
    emitInstr "leave"
    emitInstr "ret"


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


addDecls :: Type_ -> [Item_] -> CS ()

addDecls type_ items = forM_ items $ addDecl type_


getVar :: String -> CS String

getVar ident = do
    loc <- gets $ M.lookup ident . locVars
    case loc of
        Just pos -> return pos
        Nothing -> do
            Just pos <- gets $ M.lookup ident . outVars
            return pos


addLocalVar :: String -> String -> CS ()

addLocalVar ident pos =
    modify $ \s -> s { locVars = M.insert ident pos (locVars s) }


addDecl :: Type_ -> Item_ -> CS ()

addDecl type_ item =
    case item of
        NoInit_ (Ident_ ident) -> case type_ of
            -- TODO
            Str_ -> addLocalVar ident ""
            --
            Int_ -> addLocalVar ident "$0"
            Bool_ -> addLocalVar ident "$0"

        Init_ (Ident_ ident) expr -> do
            res <- addExpr expr
            addLocalVar ident res


addExpr :: Expr_ -> CS String

addExpr expr =
    case expr of
        EVar_ (Ident_ eIdent) -> getVar eIdent
        ELitInt_ int -> return $ "$" ++ (show int)
        ELitTrue_ -> return "$1"
        ELitFalse_ -> return "$0"
        -- EApp_ _ _ ->
        EString_ str -> return str
        Neg_ expr -> do
            res <- addExpr expr
            emitNeg res
        Not_ expr -> do
            res <- addExpr expr
            emitNot res
        EMul_ e1 op e2 -> do
            res1 <- addExpr e1
            res2 <- addExpr e2
            emitMul res1 op res2
        -- EAdd_ e1 op e2 -> doubleExtractApps e1 e2
        -- ERel_ e1 op e2 -> doubleExtractApps e1 e2
        -- EAnd_ e1 e2 -> doubleExtractApps e1 e2
        -- EOr_ e1 e2


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


chooseReg :: String -> String -> String -> CS (String, String)

chooseReg pos1 pos2 def =
    case pos1 of
        '%':_ -> return (pos1, pos2)
        _ -> case pos2 of
            '%':_ -> return (pos2, pos1)
            _ -> do
                tryMovl pos1 def
                return (def, pos2)


emitMul ::  String -> MulOp_ -> String -> CS String

emitMul pos1 op pos2 = do
    (res, pos) <- chooseReg pos1 pos2 "%eax"
    case op of
        Times_ -> do
            emitDouble "imull" pos res
            return res



emitNeg :: String -> CS String

emitNeg pos = do
    let res = "%eax"
    finRes <- tryMovl pos res
    emitDouble "imull" "$-1" finRes
    return finRes


emitNot :: String -> CS String

emitNot pos = do
    let res = "%eax"
    finRes <- tryMovl pos res
    emitDouble "xorl" "$1" finRes
    return finRes


addAss :: Ident_ -> Expr_ -> CS ()

addAss (Ident_ ident) expr = do
    res <- addExpr expr
    addLocalVar ident res


incr :: Ident_ -> Integer -> CS ()

incr (Ident_ ident) int = do
    var <- getVar ident
    case var of
        '$':prevInt ->
            addLocalVar ident $ "$" ++ (show ((read prevInt :: Integer) + int))
        _ -> do
            singleOccurence <- checkMultiple ident var
            let instr = if int == 1
                then "incl"
                else "decl"
            if singleOccurence
                then emitSingle instr var
                else do
                    pos <- addStack ident
                    emitSingle instr pos


addStack :: String -> CS String

addStack ident = do
    modify $ \s -> s { stackEnd = (stackEnd s + 4) }
    loc <- gets $ M.lookup ident . locVars
    offset <- gets stackEnd
    let pos = (show offset) ++ "(%ebp)"
    case loc of
        Just pos -> modify $ \s -> s { locVars = M.insert ident pos (locVars s) }
        Nothing -> modify $ \s -> s { outVars = M.insert ident pos (outVars s) }
    return pos


emitInstr :: String -> CS ()

emitInstr instr =
    let line = "\t" ++ instr
    in addLines [line]


emitSingle :: String -> String -> CS ()

emitSingle instr arg =
    let line = "\t" ++ instr ++ "\t" ++ arg
    in addLines [line]


emitDouble :: String -> String -> String -> CS ()

emitDouble instr arg1 arg2 =
    let line = "\t" ++ instr ++ "\t" ++ arg1 ++ ", " ++ arg2
    in addLines [line]


-- findAvailable :: CS String
--
-- findAvailable = do
--     let pred (k, v) = v == Nothing
--     registers <- gets regs
--     case filter pred $ M.toList registers of
--         reg:regs -> return reg
--         [] -> do
--             Just var <- M.lookup "%eax" registers
--
--             return "eax"
--

checkMultiple :: String -> String -> CS Bool

checkMultiple ident pos = do
    let pred (k, v) = (k /= ident) && (pos == v)
    loc <- gets locVars
    out <- gets outVars
    let
        filteredLoc = filter pred $ M.toList loc
        filteredOut = filter pred $ M.toList out
    return $ (filteredLoc == []) && (filteredOut == [])
