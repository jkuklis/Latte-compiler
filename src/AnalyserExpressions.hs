module AnalyserExpressions where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte

import AbstractTree

import AnalyserUtility
import AnalyserErrors
import AnalyserClasses


checkExprs :: [Expr Pos] -> AS ()

checkExprs [] =
    return ()

checkExprs (expr:exprs) = do
    checkExpr expr
    checkExprs exprs


checkExpr :: Expr Pos -> AS (Maybe (Type Pos))

checkExpr expr = case expr of
    EVar pos ident -> do
        var <- findVar pos ident
        case var of
            Just (vPos, vType) ->
                return $ Just vType
            Nothing -> do
                msgVarUndefined ident pos
                return Nothing

    ELitInt pos int -> do
        let minInt = -2147483648
        let maxInt = 2147483647
        if minInt > int
            then msgIntTooSmall pos int
            else when (maxInt < int) $ msgIntTooBig pos int
        return $ Just $ Int pos

    ELitTrue pos ->
        return $ Just $ Bool pos

    ELitFalse pos ->
        return $ Just $ Bool pos

    EApp pos ident exprs -> do
        fun <- gets $ M.lookup ident . funMap
        case fun of
            Just _ -> checkApp fun ident pos exprs
            Nothing -> checkExpr $ EMSelf pos ident exprs

    EString pos str ->
        return $ Just $ Str pos

    EElem pos ident expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultInt $ msgArraySize pos
        getArrayType pos ident

    ENull pos ident -> checkExpr $ ENew pos ident

    EArrayNew pos type_ expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultInt $ msgArraySize pos
        case type_ of
            Class cPos cIdent -> do
                checkExpr $ ENew cPos cIdent
                return $ Just $ Array pos type_
            _ ->
                return $ Just $ Array pos type_

    ENew pos ident -> do
        class_ <- getClassProto ident
        if class_ == Nothing
            then do
                msgClassUndefined ident pos
                return Nothing
            else
                return $ Just $ Class pos ident

    EAttr pos object attr -> do
        obj <- findVar pos object
        case obj of
            Just (pPos, Array aPos type_) ->
                case attr of
                    Ident "length" -> return $ Just $ Int pos
                    _ -> getObjectAttrType pos object attr
            _ -> getObjectAttrType pos object attr

    EMethod pos object method exprs -> do
        fun <- getObjectMethod pos object method
        checkApp fun method pos exprs

    EASelf pos attr -> do
        class_ <- gets curClass
        case class_ of
            Nothing -> do
                msgSelfAttr pos attr
                return Nothing
            Just _ ->
                getClassAttrType pos class_ attr

    EMSelf pos method exprs -> do
        class_ <- gets curClass
        case class_ of
            Nothing -> do
                msgSelfMethod pos method
                return Nothing
            Just _ -> do
                fun <- getClassMethod pos class_ method
                checkApp fun method pos exprs

    Neg pos expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultInt $ msgNeg pos
        return $ Just $ Int pos

    Not pos expr -> do
        eType <- checkExpr expr
        checkTypes eType defaultBool $ msgNot pos
        return $ Just $ Bool pos

    EMul pos e1 op e2 -> do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultInt $ msgMul pos 1
        checkTypes eType2 defaultInt $ msgMul pos 2
        return $ Just $ Int pos

    EAdd pos e1 op e2 ->
        checkAdd expr

    ERel pos e1 op e2 -> do
        checkRel expr
        return $ Just $ Bool pos

    EAnd pos e1 e2 -> do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultBool $ msgAnd pos 1
        checkTypes eType2 defaultBool $ msgAnd pos 2
        return $ Just $ Bool pos

    EOr pos e1 e2 -> do
        eType1 <- checkExpr e1
        eType2 <- checkExpr e2
        checkTypes eType1 defaultBool $ msgOr pos 1
        checkTypes eType2 defaultBool $ msgOr pos 2
        return $ Just $ Bool pos


checkRel :: Expr Pos -> AS ()

checkRel (ERel pos e1 op e2) =
    case op of
        EQU oPos-> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            case eType1 of
                Just eType1 -> do
                    placeHintType oPos eType1
                    case eType1 of
                        Void _ -> msgVoidComp oPos
                        _ -> return ()
                    checkTypes eType2 eType1 $ msgEqType pos eType1

                Nothing -> do
                    return ()

        NE oPos ->
            checkRel (ERel pos e1 (EQU oPos) e2)

        _ -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            checkTypes eType1 defaultInt $ msgRelInt pos 1
            checkTypes eType2 defaultInt $ msgRelInt pos 2


checkAdd :: Expr Pos -> AS (Maybe (Type Pos))

checkAdd (EAdd pos e1 op e2) =
    case op of
        Plus (Just oPos) -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            case eType1 of
                Just (Int ePos) -> do
                    case eType2 of
                        Just (Int ePos) ->
                            return ()
                        Nothing ->
                            return ()
                        Just eType2 ->
                            msgAddInt pos 2 eType2
                    placeHint oPos Int_
                    return $ Just $ Int pos

                Just (Str ePos) -> do
                    case eType2 of
                        Just (Str ePos) ->
                            return ()
                        Nothing ->
                            return ()
                        Just eType2 ->
                            msgAddStr pos 2 eType2
                    placeHint oPos Str_
                    return $ Just $ Str pos

                Just eType1 -> case eType2 of
                    Just (Int ePos) -> do
                        msgAddInt pos 1 eType1
                        return $ Just $ Int pos

                    Just (Str ePos) -> do
                        msgAddStr pos 1 eType1
                        return $ Just $ Str pos

                    Nothing -> do
                        msgAddType pos
                        return Nothing

                Nothing -> case eType2 of
                    Just (Int ePos) ->
                        return $ Just $ Int pos

                    Just (Str ePos) ->
                        return $ Just $ Str pos

                    Nothing -> do
                        msgAddType pos
                        return Nothing

        Minus oPos -> do
            eType1 <- checkExpr e1
            eType2 <- checkExpr e2
            checkTypes eType1 defaultInt $ msgMinus pos 1
            checkTypes eType2 defaultInt $ msgMinus pos 2
            return $ Just $ Int pos


checkApp :: Maybe FunProto -> Ident -> Pos -> [Expr Pos] -> AS (Maybe (Type Pos))

checkApp fun ident pos exprs =
    case fun of
        Just (fPos, fType, args) -> do
            checkArgs ident pos args exprs
            return $ Just fType
        Nothing -> do
            msgFunUndefined ident pos
            return Nothing


checkArgs :: Ident -> Pos -> [Arg Pos] -> [Expr Pos] -> AS ()

checkArgs _ _ [] [] =
    return ()

checkArgs ident pos args [] =
    msgTooFewArgs ident pos args

checkArgs ident pos [] exprs = do
    msgTooManyArgs ident pos exprs
    checkExprs exprs

checkArgs ident pos ((Arg _ aType aIdent):args) (expr:exprs) = do
    eType <- checkExpr expr
    checkTypes eType aType $ msgArgType pos ident aIdent aType
    checkArgs ident pos args exprs


checkTypes :: Maybe (Type Pos) -> Type Pos -> (Type Pos -> AS ()) -> AS ()

checkTypes eType dType action = case eType of
    Nothing ->
        return ()
    Just (eType) -> do
        classes <- gets classMap
        when (not (cmpTypes dType eType classes)) $ action eType


constantBool :: Expr Pos -> AS (Maybe Bool)

constantBool expr = case expr of
    EVar _ _ ->
        return Nothing

    ELitTrue _ ->
        return $ Just True

    ELitFalse _ ->
        return $ Just False

    EApp _ _ _ ->
        return Nothing

    Not _ expr -> do
        con <- constantBool expr
        case con of
            Just bool ->
                return $ Just $ not bool
            Nothing ->
                return Nothing

    ERel _ e1 op e2 ->
        let
            sameVal con1 con2 =
                case (con1, con2) of
                    (Just v1, Just v2) ->
                        return $ Just $ v1 == v2
                    _ ->
                        return Nothing

        in case op of
            EQU _ -> do
                Just eType <- checkExpr e1
                case eType of
                    Int _ -> do
                        con1 <- constantInt e1
                        con2 <- constantInt e1
                        sameVal con1 con2

                    Bool _ -> do
                        con1 <- constantBool e1
                        con2 <- constantBool e1
                        sameVal con1 con2

                    Str _ -> do
                        con1 <- constantStr e1
                        con2 <- constantStr e2
                        sameVal con1 con2

                    _ -> return Nothing

            NE pos -> do
                con <- constantBool (ERel pos e1 (EQU pos) e2)
                case con of
                    Just bool ->
                        return $ Just $ not bool
                    Nothing ->
                        return Nothing

            _ -> do
                con1 <- constantInt e1
                con2 <- constantInt e2
                sameVal con1 con2

    EAnd _ e1 e2 -> do
        con1 <- constantBool e1
        con2 <- constantBool e2
        case (con1, con2) of
            (Just True, Just True) ->
                return $ Just True
            (Just False, _) ->
                return $ Just False
            (_, Just False) ->
                return $ Just False
            _ ->
                return Nothing

    EOr _ e1 e2 -> do
        con1 <- constantBool e1
        con2 <- constantBool e2
        case (con1, con2) of
            (Just False, Just False) ->
                return $ Just False
            (Just True, _) ->
                return $ Just True
            (_, Just True) ->
                return $ Just True
            _ ->
                return Nothing

    _ ->
        return Nothing


constantInt :: Expr Pos -> AS (Maybe Integer)

constantInt expr = case expr of
    EVar _ _ ->
        return Nothing

    ELitInt _ int ->
        return $ Just int

    EApp _ _ _ ->
        return Nothing

    Neg _ expr -> do
        con <- constantInt expr
        case con of
            Just int ->
                return $ Just $ negate int
            _ ->
                return Nothing

    EMul _ e1 op e2 -> do
        con1 <- constantInt e1
        con2 <- constantInt e2
        case (con1, con2) of
            (Just i1, Just i2) ->
                case op of
                    Times _ ->
                        return $ Just $ i1 * i2
                    Div pos ->
                        if i2 == 0
                            then do
                                msgDivZero pos
                                return Nothing
                            else
                                return $ Just $ i1 `div` i2
                    Mod pos ->
                        if i2 == 0
                            then do
                                msgModZero pos
                                return Nothing
                            else
                                return $ Just $ i1 `mod` i2
            _ ->
                return Nothing

    EAdd _ e1 op e2 -> do
        con1 <- constantInt e1
        con2 <- constantInt e2
        case (con1, con2) of
            (Just i1, Just i2) ->
                case op of
                    Plus _ ->
                        return $ Just $ i1 + i2
                    Minus _ ->
                        return $ Just $ i1 - i2
            _ ->
                return Nothing

    _ ->
        return Nothing


constantStr :: Expr Pos -> AS (Maybe String)

constantStr expr = case expr of
    EVar _ _ ->
        return Nothing

    EApp _ _ _->
        return Nothing

    EString _ str ->
        return $ Just str

    EAdd pos e1 op e2 ->  do
        con1 <- constantStr e1
        con2 <- constantStr e2
        case (con1, con2) of
            (Just str1, Just str2) ->
                return $ Just $ str1 ++ str2
            _ ->
                return Nothing

    _ ->
        return Nothing
