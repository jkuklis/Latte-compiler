module ClassMapConverter where

import Control.Monad.State

import qualified Data.Map as M

import AbsLatte

import AnalyserUtility


type MMap = [(Ident, Ident)]

type VMap = [(Ident, Ident)]

type ConvClassMap = M.Map Ident (VMap, MMap)


convertClassMap :: ClassMap -> ConvClassMap

convertClassMap classMap =
    M.foldrWithKey (convertClass classMap) M.empty classMap


convertClass :: ClassMap -> Ident -> ClassProto -> ConvClassMap -> ConvClassMap

convertClass classMap ident proto@(_, funMap, varMap, extended) convClassMap =
    case M.lookup ident convClassMap of
        Just _ -> convClassMap
        Nothing -> case extended of
            Just extended ->
                let extendedMembers = M.lookup extended convClassMap
                in case extendedMembers of
                    Just (vMap, mMap) ->
                        let attrs = M.foldrWithKey (addAttribute ident) [] varMap
                            methods = M.foldrWithKey (addMethod ident) [] funMap
                            newVMap = joinAttrs attrs vMap
                            newMMap = joinMethods methods mMap
                        in M.insert ident (newVMap, newMMap) convClassMap

                    Nothing ->
                        let Just extendedProto = M.lookup extended classMap
                            convClassMapExt = convertClass classMap extended extendedProto convClassMap
                        in convertClass classMap ident proto convClassMapExt
            Nothing ->
                let attrs = M.foldrWithKey (addAttribute ident) [] varMap
                    methods = M.foldrWithKey (addMethod ident) [] funMap
                in M.insert ident (attrs, methods) convClassMap


joinAttrs :: VMap -> VMap -> VMap

joinAttrs attr vMap = concat [attr, vMap]


joinMethods :: MMap -> MMap -> MMap

joinMethods methods mMap =
    foldr joinMethod methods mMap


joinMethod :: (Ident, Ident) -> MMap -> MMap

joinMethod (class_, method) mMap =
    if has method mMap
        then mMap
        else concat [mMap, [(class_, method)]]


has :: Ident -> MMap -> Bool

has ident [] = False

has ident1 ((_, ident2) : mMap) =
    if ident1 == ident2
        then True
        else has ident1 mMap


addAttribute :: Ident -> Ident -> VarProto -> VMap -> VMap

addAttribute class_ attr _ vMap =
    (class_, attr) : vMap


addMethod :: Ident -> Ident -> FunProto -> MMap -> MMap

addMethod class_ method _ mMap =
    (class_, method) : mMap
