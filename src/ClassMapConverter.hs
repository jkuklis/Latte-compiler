module ClassMapConverter where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte

import AnalyserUtility
import AbstractTree


type MMap = [(Ident_, Ident_)]

type AttrProto = (Ident_, Ident_, Type_)

type VMap = [AttrProto]

type ClassTable = (VMap, MMap)

type ConvClassMap = M.Map Ident_ ClassTable


idChange :: Ident -> Ident_

idChange (Ident ident) = Ident_ ident


convertClassMap :: ClassMap -> ConvClassMap

convertClassMap classMap =
    M.foldrWithKey (convertClass classMap) M.empty classMap


convertClass :: ClassMap -> Ident -> ClassProto -> ConvClassMap -> ConvClassMap

convertClass classMap ident1 proto@(_, funMap, varMap, extended1) convClassMap =
    let ident = idChange ident1
    in case M.lookup ident convClassMap of
        Just _ -> convClassMap
        Nothing -> case extended1 of
            Just extended1 ->
                let
                    extended = idChange extended1
                    extendedMembers = M.lookup extended convClassMap
                in case extendedMembers of
                    Just (vMap, mMap) ->
                        let attrs = M.foldrWithKey (addAttribute ident) [] varMap
                            methods = M.foldrWithKey (addMethod ident) [] funMap
                            newVMap = joinAttrs attrs vMap
                            newMMap = joinMethods methods mMap
                        in M.insert ident (newVMap, newMMap) convClassMap

                    Nothing ->
                        let Just extendedProto = M.lookup extended1 classMap
                            convClassMapExt = convertClass classMap extended1 extendedProto convClassMap
                        in convertClass classMap ident1 proto convClassMapExt
            Nothing ->
                let attrs = M.foldrWithKey (addAttribute ident) [] varMap
                    methods = M.foldrWithKey (addMethod ident) [] funMap
                in M.insert ident (attrs, methods) convClassMap


joinAttrs :: VMap -> VMap -> VMap

joinAttrs attr vMap = concat [vMap, attr]


joinMethods :: MMap -> MMap -> MMap

joinMethods methods mMap =
    foldr joinMethod mMap methods


joinMethod :: (Ident_, Ident_) -> MMap -> MMap

joinMethod (class_, method) mMap =
    let (resMap, swapped) = trySwap class_ method mMap
    in if swapped
        then resMap
        else concat [mMap, [(class_, method)]]


trySwap :: Ident_ -> Ident_ -> MMap -> (MMap, Bool)

trySwap _ _ [] = ([], False)

trySwap class_ ident1 (f@(_, ident2) : mMap) =
    if ident1 == ident2
        then ((class_, ident1) : mMap, True)
        else
            let (res, swapped) = trySwap class_ ident1 mMap
            in (f : res, swapped)


addAttribute :: Ident_ -> Ident -> VarProto -> VMap -> VMap

addAttribute class_ attr (_, type_) vMap =
    (class_, idChange attr, convertType type_) : vMap


addMethod :: Ident_ -> Ident -> FunProto -> MMap -> MMap

addMethod class_ method _ mMap =
    (class_, (idChange method)) : mMap
