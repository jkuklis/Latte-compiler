module Compiler where

import Control.Monad.State

import qualified Data.List as L

import AbstractTree
import CompilerUtility


compile :: Program_ -> IO ()

compile (Program_ defs) = do
    let state = execState (compDefs defs) startState
    putStrLn $ unlines $ reverse $ heap state
    putStrLn $ unlines $ reverse $ code state


compDefs :: [TopDef_] -> CS ()

compDefs defs = forM_ defs compDef


compDef :: TopDef_ -> CS ()

compDef (FnDef_ type_ ident args block) = do
    addFun ident
    zeroStack
    clearArgs
    addArgs args
    addBlock block
    checkVoidRet type_
    moveFrame


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
    res <- addExpr expr
    var <- getVar ident
    case (res, var) of
        ('%':_, _) ->
            emitDouble "movl" res var
        ('$':_, _) ->
            emitDouble "movl" res var
        (_, _) -> do
            let tmp = "%eax"
            emitDouble "movl" res tmp
            emitDouble "movl" tmp var


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


checkVoidRet :: Type_ -> CS ()

checkVoidRet type_ = do
    (line:lines) <- gets code
    if (type_ == Void_) && (not ("\tret" `L.isPrefixOf` line))
        then voidRet
        else return ()


pushArgs :: [Expr_] -> CS ()

pushArgs exprs = forM_ exprs pushArg


pushArg :: Expr_ -> CS ()

pushArg expr = do
    res <- addExpr expr
    emitSingle "push" res
