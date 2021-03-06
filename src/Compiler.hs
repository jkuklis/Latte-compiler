module Compiler where

import Control.Monad.State

import qualified Data.List as L

import AbsLatte

import AbstractTree
import CompilerUtility
import ClassMapConverter


compile :: Program_ -> ConvClassMap -> IO ()

compile (Program_ defs) classMap = do
    -- putStrLn $ show $ Program_ defs
    let state = execState (compDefs defs) (startState classMap)
        virtualTables = compileVirtualTables classMap
    putStrLn $ unlines $ reverse $ heap state
    putStrLn $ unlines $ virtualTables
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
    when (type_ == Void_) checkVoidRet
    checkEmptyLabel
    moveFrame
    saveFunCode

compDef (ClDef_ ident block) = do
    setCurClass ident
    addClassBlock ident block

compDef (ClInher_ ident extended classBlock) = do
    setCurClass ident
    addClassBlock ident classBlock


addBlock :: Block_ -> CS ()

addBlock (Block_ stmts) = addStmts stmts


addClassBlock :: Ident_ -> ClBlock_ -> CS ()

addClassBlock ident (ClBlock_ members) = addMembers ident members


addMembers :: Ident_ -> [ClMember_] -> CS ()

addMembers ident members =
    mapM_ (addMember ident) members


addMember :: Ident_ -> ClMember_ -> CS ()

addMember ident (ClAttr_ type_ attrIdent) =
    return ()

addMember ident (ClFun_ type_ funIdent args block) = do
    newIdent <- mergeIdents ident funIdent
    args <- appendSelf ident args
    compDef $ FnDef_ type_ newIdent args block


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

        ElemAss_ ident index expr ->
            addElemAss ident index expr

        AttrAss_ class_ object attr expr ->
            addAttrAss class_ object attr expr

        SelfAtAss_ attr expr ->
            addSelfAtAss attr expr

        Incr_ ident ->
            incr ident 1

        Decr_ ident ->
            incr ident (-1)

        SelfIncr_ ident ->
            selfIncr ident 1

        SelfDecr_ ident ->
            selfIncr ident (-1)

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
    res <- tryMovl res "%eax"
    emitDouble "cmp" "$0" res
    emitSingle "je" label
    addStmt stmt
    addLabel label


addCondElse :: Expr_ -> Stmt_ -> Stmt_ -> CS ()

addCondElse expr stmt1 stmt2 = do
    lFalse <- nextLabel
    lEnd <- nextLabel
    res <- addExpr expr
    res <- tryMovl res "%eax"
    emitDouble "cmp" "$0" res
    emitSingle "je" lFalse
    addStmt stmt1
    ret <- lastLineRet
    when (not ret) $
        emitSingle "jmp" lEnd
    addLabel lFalse
    addStmt stmt2
    when (not ret) $
        addLabel lEnd


addWhile :: Expr_ -> Stmt_ -> CS ()

addWhile expr stmt =
    if expr == ELitTrue_
        then do
            lBody <- nextLabel
            addLabel lBody
            addStmt stmt
            emitSingle "jmp" lBody
        else do
            lBody <- nextLabel
            lCond <- nextLabel
            emitSingle "jmp" lCond
            addLabel lBody
            addStmt stmt
            addLabel lCond
            res <- addExpr expr
            res <- tryMovl res "%eax"
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
                Str_ -> emitDouble "movl" emptyStringLabel pos
                Int_ -> emitDouble "movl" "$0" pos
                Bool_ -> emitDouble "movl" "$0" pos
                Class_ classIdent -> emitDouble "movl" "$0" pos
                Array_ type_ -> emitDouble "movl" "$0" pos

        Init_ (Ident_ ident) expr -> do
            res <- addExpr expr
            pos <- addStack ident
            case res of
                '%':_ ->
                    strictMovl res pos
                '$':_ ->
                    strictMovl res pos
                _ -> do
                    let tmp = "%eax"
                    strictMovl res tmp
                    strictMovl tmp pos
            return ()


addExpr :: Expr_ -> CS String

addExpr expr =
    let methodCont class_ object method exprs = do
        met <- getMethod class_ object method
        emitSingle "pushl" object
        emitSingle "call" met
        restoreEspLen $ toInteger $ 1 + length exprs
        return "%eax"

    in case expr of
        EVar_ (Ident_ eIdent) -> getVar eIdent
        ELitInt_ int -> return $ "$" ++ (show int)
        ELitTrue_ -> return "$1"
        ELitFalse_ -> return "$0"
        EApp_ (Ident_ fIdent) exprs -> do
            pushArgs $ reverse exprs
            emitSingle "call" fIdent
            restoreEsp exprs
            return "%eax"
        EString_ str ->
            case str of
                "" -> return emptyStringLabel
                _ -> do
                    res <- addToHeap str
                    return $ "$" ++ res
        EElem_ ident expr -> do
            res <- addExpr expr
            emitElem ident res
        ELength_ (Ident_ array) -> do
            arrayAddress <- getVar array
            regAddress <- tryMovl arrayAddress "%eax"
            return $ "(" ++ regAddress ++ ")"
        ENull_ ident -> return $ "$0"
        EArrayNew_ type_ expr -> do
            res <- addExpr expr
            emitNewArray type_ res
        ENew_ ident -> emitNew ident
        EASelf_ (Ident_ attr) -> getVar attr
        EMSelf_ method exprs -> do
            pushArgs $ reverse exprs
            class_ <- gets curClass
            let object = selfObject
            methodCont class_ object method exprs
        EAttr_ class_ (Ident_ object) attr -> do
            obj <- getVar object
            getAttribute class_ obj attr notRealReg
        EMethod_ class_ (Ident_ object) method exprs -> do
            pushArgs $ reverse exprs
            object <- getVar object
            methodCont class_ object method exprs
        Neg_ expr -> do
            res <- addExpr expr
            emitNeg res
        Not_ expr -> do
            res <- addExpr expr
            emitNot res
        EMul_ e1 op e2 -> emitDoubleExpr e1 e2 $ emitMul op
        EAdd_ e1 op e2 -> emitDoubleExpr e1 e2 $ emitAdd op
        EStrAdd_ e1 e2 -> do
            pushArgs [e2, e1]
            emitSingle "call" "_concatenate"
            restoreEspLen 2
            return "%eax"
        ERel_ e1 op e2 -> emitDoubleExpr e1 e2 $ emitRel op
        EAnd_ e1 e2 -> emitAnd e1 e2
        EOr_ e1 e2 -> emitOr e1 e2


emitDoubleExpr :: Expr_ -> Expr_ -> (String -> String -> CS String) -> CS String

emitDoubleExpr e1 e2 cont = do
    let noHelperCont res1 e2 cont = do
        res2 <- addExpr e2
        cont res1 res2

    res1 <- addExpr e1
    case e2 of
        EVar_ _ -> noHelperCont res1 e2 cont
        ELitInt_ _ -> noHelperCont res1 e2 cont
        ELitTrue_ -> noHelperCont res1 e2 cont
        ELitFalse_ -> noHelperCont res1 e2 cont
        EString_ _ -> noHelperCont res1 e2 cont
        _ -> do
            helper <- addHelper res1
            res2 <- addExpr e2
            if (helper /= res1)
                then do
                    res1Back <- getHelper helper res2 "%eax" "%ecx"
                    cont res1Back res2
                else
                    cont res1 res2


emitElem :: Ident_ -> String -> CS String

emitElem (Ident_ ident) res = do
    res <- tryMovl res "%eax"
    getArrayElem ident res notRealReg


emitNewArray :: Type_ -> String -> CS String

emitNewArray type_ res = do
    res <- tryMovl res "%eax"
    helper <- addHelper res
    emitSingle "incl" res
    callCalloc res
    getHelperStrict helper "%ecx"
    emitDouble "movl" "%ecx" "(%eax)"
    case type_ of
        Str_ -> do
            let addressAux = "%edx"
            lBody <- nextLabel
            lCond <- nextLabel
            emitSingle "jmp" lCond
            addLabel lBody
            emitDouble "leal" "(%eax, %ecx, 4)" "%edx"
            emitDouble "movl" emptyStringLabel "(%edx)"
            emitSingle "decl" "%ecx"
            addLabel lCond
            emitDouble "cmp" "$0" "%ecx"
            emitSingle "jg" lBody
        _ -> return ()
    return "%eax"


emitNew :: Ident_ -> CS String

emitNew ident@(Ident_ ident_) = do
    (vMap, _) <- getClassTable ident
    let size = 1 + length vMap
        sizeConst = "$" ++ (show size)
        label = "$__" ++ ident_
    callCalloc sizeConst
    emitDouble "movl" label "(%eax)"
    initStringAttributes vMap
    return "%eax"


emitMul :: MulOp_ -> String -> String -> CS String

emitMul op pos1 pos2 =
    let
        dividend = "%eax"
        divisor = "%ecx"
        auxiliary = "%edx"
        divisionHelper = do
            divisorPos <- setDivision pos1 pos2 dividend divisor auxiliary
            emitDouble "movl" "$0" "%edx"
            emitSingle "idivl" divisorPos
    in case op of
        Times_ -> do
            (res, pos) <- chooseReg pos1 pos2 "%eax"
            emitDouble "imull" pos res
            return res
        Div_ -> do
            divisionHelper
            return "%eax"
        Mod_ -> do
            divisionHelper
            return "%edx"


emitAdd :: AddOp_ -> String -> String -> CS String

emitAdd op pos1 pos2 =
    case op of
        Plus_ -> do
            (res, pos) <- chooseReg pos1 pos2 "%eax"
            emitDouble "addl" pos res
            return res
        Minus_ -> do
            res <- setSubtract pos1 pos2 "%ecx" "%edx"
            emitDouble "subl" pos2 res
            return res


emitRel :: RelOp_ -> String -> String -> CS String

emitRel op pos1 pos2 =
    let
        stringsEquality neq = do
            emitSingle "pushl" pos2
            emitSingle "pushl" pos1
            emitSingle "call" "strcmp"
            when (neq == False) $
                emitSingle "not" "%eax"
            restoreEspLen 2
            return "%eax"

    in case op of
        EQU_ Str_ -> stringsEquality False
        NE_ Str_ -> stringsEquality True
        _ -> if pos1 == pos2
            then case op of
                LTH_ -> return "$0"
                LE_ -> return "$1"
                GTH_ -> return "$0"
                GE_ -> return "$1"
                EQU_ _ -> return "$1"
                NE_ _ -> return "$0"

            else do
                label <- nextLabel
                let
                    switcher p1 p2 r1 r2 r3 =
                        let
                            p1_ = extractReg p1
                            p2_ = extractReg p2
                        in if (p1_ == r1) || (p2_ == r1)
                            then if (p1_ == r2) || (p2_ == r2)
                                then r3
                                else r2
                            else r1

                    res = switcher pos1 pos2 "%eax" "%ecx" "%edx"
                    aux1 = switcher pos2 res "%eax" "%ecx" "%edx"
                    aux2 = switcher pos1 res "%eax" "%ecx" "%edx"

                    instr = case op of
                        LTH_ -> "jl"
                        LE_ -> "jle"
                        GTH_ -> "jg"
                        GE_ -> "jge"
                        EQU_ _ -> "je"
                        NE_ _ -> "jne"

                    revInstr = case op of
                        LTH_ -> "jge"
                        LE_ -> "jg"
                        GTH_ -> "jle"
                        GE_ -> "jl"
                        EQU_ _ -> "je"
                        NE_ _ -> "jne"

                emitDouble "movl" "$1" res

                case (isConstant pos1, isConstant pos2) of
                    (True, False) -> do
                        helper <- tryMovl pos2 aux2
                        emitDouble "cmp" pos1 helper
                        emitSingle revInstr label

                    (False, False) -> do
                        if (isRegister pos1) || (isRegister pos2)
                            then do
                                emitDouble "cmp" pos2 pos1
                                emitSingle instr label
                            else do
                                helper <- tryMovl pos1 aux1
                                emitDouble "cmp" pos2 helper
                                emitSingle instr label

                    _ -> do
                        helper <- tryMovl pos1 aux1
                        emitDouble "cmp" pos2 helper
                        emitSingle instr label

                emitDouble "movl" "$0" res
                addLabel label
                return res


emitAnd :: Expr_ -> Expr_ -> CS String

emitAnd e1 e2 = do
    lEnd <- nextLabel
    res1 <- addExpr e1
    reg <- tryMovl res1 "%eax"
    emitDouble "cmp" "$0" reg
    emitSingle "je" lEnd
    res2 <- addExpr e2
    strictMovl res2 reg
    addLabel lEnd
    return reg


emitOr :: Expr_ -> Expr_ -> CS String

emitOr e1 e2 = do
    lEnd <- nextLabel
    res1 <- addExpr e1
    reg <- tryMovl res1 "%eax"
    emitDouble "cmp" "$1" reg
    emitSingle "je" lEnd
    res2 <- addExpr e2
    strictMovl res2 reg
    addLabel lEnd
    return reg


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
    var <- getVarWithTaken ident res
    transferValues res var


addElemAss :: Ident_ -> Expr_ -> Expr_ -> CS ()

addElemAss (Ident_ ident) index expr =
    let cont exprVal indexVal = do
        let
            aux = case extractReg exprVal of
                "%eax" -> "%ecx"
                _ -> "%eax"
        indexVal <- tryMovl indexVal aux
        elemAddress <- getArrayElem ident indexVal exprVal
        transferValues exprVal elemAddress
        return elemAddress
    in do
        emitDoubleExpr expr index cont
        return ()


addAttrAss :: Ident_ -> Ident_ -> Ident_ -> Expr_ -> CS ()

addAttrAss class_ (Ident_ object) attr expr = do
    res <- addExpr expr
    obj <- getVarWithTaken object res
    att <- getAttribute class_ obj attr res
    transferValues res att


addSelfAtAss :: Ident_ -> Expr_ -> CS ()

addSelfAtAss attr expr = do
    res <- addExpr expr
    class_ <- gets curClass
    att <- getAttribute class_ selfObject attr res
    transferValues res att


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


selfIncr :: Ident_ -> Integer -> CS ()

selfIncr (Ident_ ident) int = do
    class_ <- gets curClass
    attr <- getAttribute class_ selfObject (Ident_ ident) notRealReg
    let instr = if int == 1
        then "incl"
        else "decl"
    emitSingle instr attr


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


checkVoidRet :: CS ()

checkVoidRet = do
    ret <- lastLineRet
    if not ret
        then voidRet
        else return ()


pushArgs :: [Expr_] -> CS ()

pushArgs exprs = forM_ exprs pushArg


pushArg :: Expr_ -> CS ()

pushArg expr = do
    res <- addExpr expr
    emitSingle "pushl" res
