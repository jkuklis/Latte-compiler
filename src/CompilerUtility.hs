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
    heap :: [String],
    regs :: RegMap,
    locVars :: VarMap,
    outVars :: VarMap,
    stackEnd :: Integer,
    labelsCount :: Integer,
    stringsCount :: Integer,
    helpers :: Integer
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
    code = [".text"],
    heap = [".section .rodata"],
    regs = registersMap,
    locVars = M.empty,
    outVars = M.empty,
    stackEnd = 0,
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
    -- emitSingle "pushl" "%esi"
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
    let reg = (show pos) ++ "(" ++ stack ++ ")"
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

        Cond_ expr stmt ->
            addCond expr stmt

        CondElse_ expr stmt1 stmt2 ->
            addCondElse expr stmt1 stmt2

        While_ expr stmt ->
            addWhile expr stmt

        SExp_ expr -> do
            addExpr expr
            return ()


checkVoidRet :: Type_ -> CS ()

checkVoidRet type_ = do
    (line:lines) <- gets code
    if (type_ == Void_) && (not ("\tret" `L.isPrefixOf` line))
        then voidRet
        else return ()


voidRet :: CS ()

voidRet = do
    -- emitSingle "popl" "%esi"
    emitInstr "leave"
    emitInstr "ret"


exprRet :: Expr_ -> CS ()

exprRet expr = do
    pos <- addExpr expr
    if pos == "%eax"
        then return ()
        else emitDouble "movl" pos "%eax"
    -- emitSingle "popl" "%esi"
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


addLabel :: String -> CS ()

addLabel label = addLines [label ++ ":"]


addCond :: Expr_ -> Stmt_ -> CS ()

addCond expr stmt = do
    label <- nextLabel
    res <- addExpr expr
    emitDouble "cmp" "$0" res
    emitSingle "je" label
    addStmt stmt
    addLabel label


addCondElse :: Expr_ -> Stmt_ -> Stmt_ -> CS ()

addCondElse expr stmt1 stmt2 = do
    lFalse <- nextLabel
    lEnd <- nextLabel
    res <- addExpr expr
    emitDouble "cmp" "$0" res
    emitSingle "je" lFalse
    addStmt stmt1
    emitSingle "jmp" lEnd
    addLabel lFalse
    addStmt stmt2
    addLabel lEnd


addWhile :: Expr_ -> Stmt_ -> CS ()

addWhile expr stmt = do
    lBody <- nextLabel
    lCond <- nextLabel
    emitSingle "jmp" lCond
    addLabel lBody
    addStmt stmt
    addLabel lCond
    res <- addExpr expr
    emitDouble "cmp" "$1" res
    emitSingle "je" lBody


addDecls :: Type_ -> [Item_] -> CS ()

addDecls type_ items = forM_ items $ addDecl type_


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


addDecl :: Type_ -> Item_ -> CS ()

addDecl type_ item =
    case item of
        NoInit_ (Ident_ ident) -> do
            pos <- addStack ident
            case type_ of
                Str_ -> do
                    res <- addToHeap ""
                    emitDouble "movl" ("$" ++ res) pos
                Int_ -> emitDouble "movl" "$0" pos
                Bool_ -> emitDouble "movl" "$0" pos

        Init_ (Ident_ ident) expr -> do
            res <- addExpr expr
            pos <- addStack ident
            case res of
                '%':_ ->
                    emitDouble "movl" res pos
                '$':_ ->
                    emitDouble "movl" res pos
                _ -> do
                    let tmp = "%eax"
                    emitDouble "movl" res tmp
                    emitDouble "movl" tmp pos



addExpr :: Expr_ -> CS String

addExpr expr =
    let doubleExpr e1 e2 cont = do
        res1 <- addExpr e1
        helper <- addHelper res1
        res2 <- addExpr e2
        helperReg <- getHelper helper res2
        cont helper res2
    in case expr of
        EVar_ (Ident_ eIdent) -> getVar eIdent
        ELitInt_ int -> return $ "$" ++ (show int)
        ELitTrue_ -> return "$1"
        ELitFalse_ -> return "$0"
        EApp_ (Ident_ fIdent) exprs -> do
            pushArgs $ reverse exprs
            emitSingle "call" fIdent
            return "%eax"
        EString_ str -> do
            res <- addToHeap str
            return $ "$" ++ res
        Neg_ expr -> do
            res <- addExpr expr
            emitNeg res
        Not_ expr -> do
            res <- addExpr expr
            emitNot res
        EMul_ e1 op e2 -> doubleExpr e1 e2 $ emitMul op
        EAdd_ e1 op e2 -> doubleExpr e1 e2 $ emitAdd op
        EStrAdd_ e1 e2 -> do
            pushArgs [e2, e1]
            emitSingle "call" "_concatenate"
            return "%eax"
        ERel_ e1 op e2 -> doubleExpr e1 e2 $ emitRel op
        EAnd_ e1 e2 -> doubleExpr e1 e2 $ emitAnd
        EOr_ e1 e2 -> doubleExpr e1 e2 $ emitOr


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


nextString :: CS String

nextString = do
    count <- gets stringsCount
    modify $ \s -> s { stringsCount = count + 1 }
    return $ ".LC" ++ (show count)


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


pushArgs :: [Expr_] -> CS ()

pushArgs exprs = forM_ exprs pushArg


pushArg :: Expr_ -> CS ()

pushArg expr = do
    res <- addExpr expr
    emitSingle "push" res


emitMul :: MulOp_ -> String -> String -> CS String

emitMul op pos1 pos2 =
    let divider = "%ecx"
    in case op of
        Times_ -> do
            (res, pos) <- chooseReg pos1 pos2 "%eax"
            emitDouble "imull" pos res
            return res
        Div_ -> do
            emitDouble "movl" "$0" "%edx"
            strictMovl pos1 "%eax"
            strictMovl pos2 divider
            emitSingle "idivl" divider
            return "%eax"
        Mod_ -> do
            emitDouble "movl" "$0" "%edx"
            strictMovl pos1 "%eax"
            strictMovl pos2 divider
            emitSingle "idivl" divider
            return "%edx"


emitAdd :: AddOp_ -> String -> String -> CS String

emitAdd op pos1 pos2 =
    case op of
        Plus_ -> do
            (res, pos) <- chooseReg pos1 pos2 "%eax"
            emitDouble "addl" pos res
            return res
        Minus_ -> do
            res <- tryMovl pos1 "%eax"
            emitDouble "subl" pos2 res
            return res


nextLabel :: CS String

nextLabel = do
    count <- gets labelsCount
    modify $ \s -> s { labelsCount = labelsCount s + 1 }
    return $ ".LF" ++ (show count)


emitRel :: RelOp_ -> String -> String -> CS String

emitRel op pos1 pos2 =
    let
        stringsEquality neq = do
            emitSingle "push" pos2
            emitSingle "push" pos1
            emitSingle "call" "strcmp"
            when (neq == False) $ emitSingle "not" "%eax"
            return "%eax"

    in case op of
        EQU_ Str_ -> stringsEquality False
        NE_ Str_ -> stringsEquality True
        _ -> do
            label <- nextLabel
            let
                (res, aux) = if pos1 == "%eax"
                    then ("%eax", "%ecx")
                    else ("%ecx", "%eax")
                instr = case op of
                    LTH_ -> "jl"
                    LE_ -> "jle"
                    GTH_ -> "jg"
                    GE_ -> "jge"
                    EQU_ _ -> "je"
                    NE_ _ -> "jne"
            helper <- tryMovl pos2 aux
            emitDouble "movl" "$1" res
            emitDouble "cmp" helper pos1
            emitSingle instr label
            emitDouble "movl" "$0" res
            addLabel label
            return res


emitAnd :: String -> String -> CS String

emitAnd pos1 pos2 = do
    (res, pos) <- chooseReg pos1 pos2 "%eax"
    emitDouble "and" pos res
    return res


emitOr :: String -> String -> CS String

emitOr pos1 pos2 = do
    (res, pos) <- chooseReg pos1 pos2 "%eax"
    emitDouble "or" pos res
    return res


emitNeg :: String -> CS String

emitNeg pos = do
    res <- tryMovl pos "%eax"
    emitSingle "neg" res
    return res


emitNot :: String -> CS String

emitNot pos = do
    res <- tryMovl pos "%eax"
    emitDouble "xorl" "$1" res
    return res


addAss :: Ident_ -> Expr_ -> CS ()

addAss (Ident_ ident) expr = do
    let
        assign res var =
            case (res, var) of
                -- ('%':_, '$':_) -> do
                --     pos <- addStack ident
                --     emitDouble "movl" res pos
                -- (_, '$':_) ->
                --     addLocalVar ident res
                ('%':_, _) ->
                    emitDouble "movl" res var
                ('$':_, _) ->
                    emitDouble "movl" res var
                (_, _) -> do
                    let tmp = "%eax"
                    emitDouble "movl" res tmp
                    emitDouble "movl" tmp var
    res <- addExpr expr
    var <- getVar ident
    assign res var





incr :: Ident_ -> Integer -> CS ()

incr (Ident_ ident) int = do
    var <- getVar ident
    let instr = if int == 1
        then "incl"
        else "decl"
    singleOccurence <- checkMultiple ident var
    if (not singleOccurence) || (head var == '$')
        then do
            pos <- addStack ident
            emitSingle instr pos
        else emitSingle instr var


addStack :: String -> CS String

addStack ident = do
    modify $ \s -> s { stackEnd = (stackEnd s + 4) }
    offset <- gets stackEnd
    let pos = "-" ++ (show offset) ++ "(%ebp)"
    addLocalVar ident pos
    return pos


zeroStack :: CS ()

zeroStack =
    modify $ \s -> s { stackEnd = 0 }


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
