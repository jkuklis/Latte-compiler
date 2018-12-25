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
stack = "ebp"
frame = "esp"

startState = CompilerState {
    code = [],
    regs = registersMap,
    locVars = M.empty,
    outVars = M.empty,
    stackEnd = 0
    }


addFun :: Ident_ -> CS ()

addFun (Ident_ ident) =
    let
        globl = "\n.globl " ++ ident
        fun = ident ++ ":"
        stackPtr = "\tpush\t%" ++ stack
        framePtr = "\tmovl\t%" ++ frame ++ ", %" ++ stack
        stackOff = "..stack_holder"
        prolog = reverse [globl, fun, stackPtr, framePtr, stackOff]

    in modify $ \s -> s { code = concat [prolog, (code s)] }


moveFrame :: CS ()

moveFrame = do
    stackHeight <- gets stackEnd
    lines <- gets code
    let
        reminder = stackHeight `mod` 16
        frame = if reminder == 0
            then stackHeight
            else stackHeight - reminder + 16
        line = if frame == 0
            then ""
            else "\tsubl\t$" ++ (show frame) ++ ", %esp"
        repLines = map (\l -> if l == "..stack_holder" then line else l) lines
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

voidRet =
    let
        leave = "\tleave"
        ret = "\tret"
    in modify $ \s -> s { code = ret : (leave : (code s)) }


exprRet :: Expr_ -> CS ()

exprRet expr = do
    pos <- addExpr expr
    let
        line = "\tmovl\t" ++ pos ++ ", %eax"
        leave = "\tleave"
        ret = "\tret"
        newLines = if pos == "%eax"
            then reverse [leave, ret]
            else reverse [line, leave, ret]
    modify $ \s -> s { code = concat [newLines, (code s)] }


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
        EString_ str -> return str


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


emitSingle :: String -> String -> CS ()

emitSingle instr var =
    let line = "\t" ++ instr ++ "\tvar"
    in modify $ \s -> s { code = line : (code s) }


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
