{-# LANGUAGE FlexibleInstances #-}
-- | Module containing Template Haskell functions for creating "keyed enums".
-- A keyed enum is a enumeration data type where each data constructor has one
-- or more associated values that it can be converted to and from.
-- For example, in Chat Core's IRC implementation, there is an enumeration type
-- for IRC commands. Each command can be converted from its string form to the
-- data type and vice versa.
-- Keyed enums are defined via the `ke` QuasiQuoter.
module ChatCore.Util.KeyedEnum
    ( mkKeyedEnum
    , mkKE

    , mkDataDec
    , mkEnumToKey
    , mkKeyToEnum

    , EnumKey (..)
    , EntryDef (..)
    , KEDecGenerator
    ) where

import Control.Applicative
import Data.Int
import Data.Word
import Data.Maybe
import Language.Haskell.TH


-- | Generates a keyed enum type and conversion functions with the given names.
-- The first argument is the data type name.
-- The second argument is the name of the function to convert keys to enum
-- values.
-- The third argument is the name of the function to do the opposite.
-- The fourth argument is either Nothing, or the name of a data constructor for
-- an "other" entry.
mkKeyedEnum :: (EnumKey k) => String -> String -> String ->
               Maybe String -> [(String, k)] -> Q [Dec]
mkKeyedEnum typeName k2eName' e2kName' otherM entries = do
    -- Make the function names.
    k2eName <- newName k2eName'
    e2kName <- newName e2kName'
    mkKE typeName otherM entries
        [ mkDataDec
        , mkKeyToEnum k2eName
        , mkEnumToKey e2kName
        ]

-- | An extended version of mkKeyedEnum that takes a list of extra AST
-- generators.
-- mkKeyedEnumExt :: (EnumKey k) => String -> String -> String ->
--                   Maybe String -> [(String, k)] -> Q [Dec]
-- mkKeyedEnumExt typeNameS k2eNameS e2kNameS otherNameStrM entries = do


-- | Type for functions that generate Haskell ASTs for KeyedEnum.
-- Argument 1 is the type name.
-- Argument 2 is the "other" data constructor's name (if applicable).
-- Argument 3 is the type name.
-- Argument 4 is the list of enum entries.
type KEDecGenerator k = Name -> Maybe Name -> Type -> [EntryDef k] -> Q [Dec]

mkKE :: (EnumKey k) =>
        String -> Maybe String -> [(String, k)] ->
        [KEDecGenerator k] -> Q [Dec]
mkKE typeName' otherM' entries' gens = do
    let typeName = mkName typeName'
    -- Make the name for the "other" data constructor if we need to.
    let otherM = maybe (Nothing) (\n -> Just $ mkName n) otherM'
    -- Generate entry def objects.
    let entries = map mkEntryDef entries'
    -- Get the key type.
    keyType <- ConT <$> (getTypeName $ edKey $ head entries)
    concat <$> mapM (\gen -> gen typeName otherM keyType entries) gens
  where
    mkEntryDef (name', key) = EntryDef (mkName name') key


-- {{{ Enum Key Class

class (Read k) => EnumKey k where
    toLit :: k -> Lit
    getTypeName :: k -> Q Name

instance EnumKey String where
    toLit = StringL
    getTypeName _ = fromJust <$> lookupTypeName "String"

instance EnumKey Integer where
    toLit = IntegerL
    getTypeName _ = fromJust <$> lookupTypeName "Integer"

instance EnumKey Int16 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Int16"

instance EnumKey Int32 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Int32"

instance EnumKey Int64 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Int64"

instance EnumKey Word16 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Word16"

instance EnumKey Word32 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Word32"

instance EnumKey Word64 where
    toLit = IntegerL . fromIntegral
    getTypeName _ = fromJust <$> lookupTypeName "Word64"

-- }}}

-- {{{ Parser

-- | Data type for keyed enum ASTs with constructed names and types.
data (EnumKey k, Read k) => EntryDef k = EntryDef
    { edName :: Name
    , edKey  :: k
    }

-- TODO: Implement QuasiQuoter.
{-
-- | Parses the given string and generates a list of `EntryDef`s from it.
parseKEDef :: (EnumKey k) => String -> [EntryDef k]
parseKEDef str = mapMaybe parseLine $ lines str
  where
    parseLine line = EntryDef <$> val <*> (read <$> keyStr)
      where
        pos = elemIndex ' ' line
        splitM = (flip splitAt) line <$> pos
        (val, keyStr) =
            case splitM of
                 Just (v, ks) -> (Just v, Just ks)
                 Nothing -> (Nothing, Nothing)
-}

-- }}}

-- {{{ Declaration Generators

-- {{{ Data type declaration

-- | Builds a data type declaration for the given entry list.
mkDataDec :: EnumKey k => KEDecGenerator k
mkDataDec name otherM cType entries = do
    showName <- fromJust <$> lookupTypeName "Prelude.Show"
    eqName <- fromJust <$> lookupTypeName "Prelude.Eq"
    typeableName <- lookupTypeName "Data.Typeable"
    let derive = [showName, eqName] ++ maybeToList typeableName
    cons <- mkDataCons otherM cType entries
    return [DataD [] name [] cons derive]

-- | Builds a list of data constructor definitions for the given entry list.
-- If the first argument is Just <name>, this function will also generate an
-- "other" type constructor with the given name.
mkDataCons :: (EnumKey k) => Maybe Name -> Type -> [EntryDef k] -> Q [Con]

mkDataCons otherM cType ((EntryDef name _) : otherDefs) =
    ((NormalC name []) :) <$> mkDataCons otherM cType otherDefs

mkDataCons (Just name) cType [] =
    return $ (NormalC name [(IsStrict, cType)]) : []

mkDataCons Nothing _ [] = return []

-- }}}

-- {{{ Key to Enum Function

-- | Takes a (Maybe) other type name, and an entry list and generates a
-- function to convert key values to enums.
mkKeyToEnum :: (EnumKey k) => Name -> KEDecGenerator k
mkKeyToEnum name _ otherM _ defs = do
    otherC  <- maybe (return []) (\o -> (:[]) <$> mkK2EOtherC o) otherM
    clauses <- (++otherC) <$> mapM mkK2EClause defs
    return [FunD name clauses]

-- | Takes a (Maybe) other type name, and an entry list and generates a list of
-- clauses for the function to convert key values to enums.
mkK2EClause :: (EnumKey k) => EntryDef k -> Q Clause
mkK2EClause (EntryDef name key) =
    return $ Clause [LitP $ toLit key] (NormalB $ ConE name) []

mkK2EOtherC :: Name -> Q Clause
mkK2EOtherC name = do
    valVar <- newName "val"
    let body = NormalB $ AppE (ConE name) (VarE valVar)
    return $ Clause [VarP valVar] (body) []

-- }}}

-- {{{ Enum to Key Function

-- | Takes a (Maybe) other type name, and an entry list and generates a
-- function to convert enums to key values.
mkEnumToKey :: (EnumKey k) => Name -> KEDecGenerator k
mkEnumToKey name _ otherM _ defs = do
    otherC  <- maybe (return []) (\o -> (:[]) <$> mkE2KOtherC o) otherM
    clauses <- (++otherC) <$> mapM mkE2KClause defs
    return [FunD name clauses]

mkE2KClause :: (EnumKey k) => EntryDef k -> Q Clause
mkE2KClause (EntryDef name key) =
    return $ Clause [ConP name []] (NormalB $ LitE $ toLit key) []

mkE2KOtherC :: Name -> Q Clause
mkE2KOtherC name = do
    valVar <- newName "val"
    let body = NormalB $ VarE valVar
        pat = ConP name [VarP valVar]
    return $ Clause [pat] body []

-- }}}

-- }}}

