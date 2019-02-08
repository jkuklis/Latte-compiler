module AnalyserClasses where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte

import AbstractTree

import AnalyserUtility
import AnalyserErrors


getClassSilent :: Pos -> Ident -> AS (Maybe (Type Pos))

getClassSilent pos object = do
    var <- findVar pos object
    case var of
        Just (_, vType) ->
            case vType of
                Class _ classIdent -> return $ Just vType
                _ -> return Nothing
        Nothing -> return Nothing


getClass :: Pos -> Ident -> AS (Maybe Ident)

getClass pos object = do
    var <- findVar pos object
    case var of
        Just (prevPos, vType) ->
            case vType of
                Class cPos classIdent ->
                    return $ Just classIdent
                _ -> do
                    msgNotClass object vType pos prevPos
                    return Nothing

        Nothing -> do
            msgVarUndeclared object pos
            return Nothing


tryGetClassProto :: Pos -> Maybe Ident -> AS (Maybe ClassProto)

tryGetClassProto pos class_ =
    case class_ of
        Just classIdent -> do
            proto <- getClassProto classIdent
            case proto of
                Just classProto ->
                    return $ Just classProto

                Nothing -> do
                    msgClassProto pos classIdent classIdent
                    return Nothing
        Nothing ->
            return Nothing


getAttributeType :: Pos -> Maybe Ident -> Ident -> AS (Maybe (Type Pos))

getAttributeType pos class_ attr = do
    proto <- tryGetClassProto pos class_
    case proto of
        Just (_, _, vMap, extended) ->
            case M.lookup attr vMap of
                Just (attrPos, attrType) ->
                    return $ Just attrType

                Nothing ->
                    getAttributeType pos extended attr

        Nothing ->
            return Nothing


getObjectAttrType :: Pos -> Ident -> Ident -> AS (Maybe (Type Pos))

getObjectAttrType pos object attr = do
    class_ <- getClass pos object
    classType <- getClassSilent pos object
    case classType of
        Nothing -> return ()
        Just classType ->
             placeHintType pos classType
    getClassAttrType pos class_ attr


getClassAttrType :: Pos -> Maybe Ident -> Ident -> AS (Maybe (Type Pos))

getClassAttrType pos class_ attr = do
    aType <- getAttributeType pos class_ attr
    case aType of
        Nothing ->
            case class_ of
                Just classIdent ->
                    msgAttributeUndefined pos classIdent attr
                _ -> return ()
        _ -> return ()
    return aType


getMethod :: Pos -> Maybe Ident -> Ident -> AS (Maybe FunProto)

getMethod pos class_ method = do
    proto <- tryGetClassProto pos class_
    case proto of
        Just (_, fMap, _, extended) ->
            let fun = M.lookup method fMap
                in case fun of
                    Just _ ->
                        return fun
                    Nothing ->
                        getMethod pos extended method
        Nothing ->
            return Nothing


getObjectMethod :: Pos -> Ident -> Ident -> AS (Maybe FunProto)

getObjectMethod pos object method = do
    class_ <- getClass pos object
    classType <- getClassSilent pos object
    case classType of
        Nothing -> return ()
        Just classType ->
             placeHintType pos classType
    getMethod pos class_ method


getClassMethod :: Pos -> Maybe Ident -> Ident -> AS (Maybe FunProto)

getClassMethod pos class_ method = do
    fun <- getMethod pos class_ method
    case fun of
        Nothing ->
            case class_ of
                Just classIdent ->
                    msgMethodUndefined pos classIdent method
                _ -> return ()
        _ -> return ()
    return fun


getArrayType :: Pos -> Ident -> AS (Maybe (Type Pos))

getArrayType pos ident = do
    var <- findVar pos ident
    case var of
        Just (vPos, vType) ->
            case vType of
                Array aPos aType ->
                    return $ Just aType
                _ -> do
                    msgNotArray ident pos
                    return Nothing
        Nothing -> do
            msgVarUndefined ident pos
            return Nothing
