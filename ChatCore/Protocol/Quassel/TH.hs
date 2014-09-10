-- A horrible, hacky version of KeyedEnum that is used for QVariant bullshit.
-- This really needs refactoring.
module ChatCore.Protocol.Quassel.TH where

import Control.Applicative
import Data.Int
import Data.Word
import Data.List
import Data.Maybe
import Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Text as T
import Language.Haskell.TH

import ChatCore.Util.KeyedEnum

-- Dummy type for the definition list which will be replaced with the QVariant
-- type.
qvType :: Type
qvType = ConT $ mkName "QVariant"

mkQVariant :: Word32 -> [(String, Word32, Type)] -> Q [Dec]
mkQVariant defKey entries'' = do
    let qvTypeName = mkName "QVariant"
        entries' = map replaceQVType entries''
        replaceQVType (n, k, TupleT 0) = (n, k, ConT qvTypeName)
        replaceQVType other = other
    -- Make the function names.
    readName <- newName "readQVForId"
    writeName <- newName "putQVariantData"
    typeIdName <- newName "qvTypeId"
    let (names, keys, types) = unzip3 entries'
        entries = zip names keys
    mkKE "QVariant" Nothing entries
        [ mkQVDec qvTypeName types
        , mkReadForId readName
        , mkWrite writeName
        , mkGetTypeId typeIdName defKey
        ]

-- {{{ Declaration Generators

-- {{{ Data type declaration

-- | Builds a QVariant data declaration for the given entry list.
mkQVDec :: EnumKey k => Name -> [Type] -> KEDecGenerator k
mkQVDec name typeEntries _ _ cType entries' = do
    showN <- fromJust <$> lookupTypeName "Prelude.Show"
    eqN <- fromJust <$> lookupTypeName "Prelude.Eq"
    typeableN <- fromJust <$> lookupTypeName "Data.Typeable.Typeable"
    let (ns, ks) = unzip $ map (\(EntryDef name key) -> (name, key)) entries'
        entries = zip3 ns ks typeEntries
    cons' <- mapM mkQVCon entries
    let cons = cons' ++ [qvInvCon]
    return [DataD [] name [] cons [showN, eqN, typeableN]]

mkQVCon :: (EnumKey k) => (Name, k, Type) -> Q Con
mkQVCon (name, keyT, contentT) =
    return $ NormalC name [(IsStrict, contentT)]

qvInvCon :: Con
qvInvCon = NormalC (mkName "QVInvalid") []

-- }}}

-- {{{ Get Type ID

mkGetTypeId :: (EnumKey k) => Name -> k -> KEDecGenerator k
mkGetTypeId name defKey _ _ _ defs = do
    invalidC <- (:[]) <$> mkTIDInvalidC (mkName "QVInvalid") defKey
    clauses <- (++invalidC) <$> mapM mkTIDClause defs
    return [FunD name clauses]

mkTIDClause :: (EnumKey k) => EntryDef k -> Q Clause
mkTIDClause (EntryDef name key) =
    return $ Clause [ConP name [WildP]] (NormalB $ LitE $ toLit key) []

mkTIDInvalidC :: (EnumKey k ) => Name -> k -> Q Clause
mkTIDInvalidC name defKey = do
    let body = NormalB $ LitE $ toLit defKey
        pat = ConP (name) []
    return $ Clause [pat] body []

-- }}}

-- {{{ Write Function

mkWrite :: Name -> KEDecGenerator Word32
mkWrite name _ _ _ defs = do
    invalidC <- (:[]) <$> mkPutInvalidC (mkName "QVInvalid")
    clauses <- (++invalidC) <$> mapM mkPutClause defs
    return [FunD name clauses]

mkPutClause :: EntryDef Word32 -> Q Clause
mkPutClause (EntryDef name key) = do
    let pqvName = mkName "putQV"
        varName = mkName "val"
        qvName = mkName "qval"
    bodyExp <- [e| $(varE pqvName) $(varE varName) |]
    pattern <- asP qvName (conP name [varP varName])
    let var = mkName "val"
        body = NormalB $ bodyExp
    return $ Clause [pattern] body []


mkPutInvalidC :: Name -> Q Clause
mkPutInvalidC name = do
    body <- NormalB <$> [e| return () |]
    return $ Clause [WildP] (body) []

-- }}}

-- {{{ Read Function

mkReadForId :: (EnumKey k) => Name -> KEDecGenerator k
mkReadForId name _ _ _ defs = do
    invalidC <- (:[]) <$> mkGetInvalidC (mkName "QVInvalid")
    clauses <- (++invalidC) <$> mapM mkGetClause defs
    return [FunD name clauses]

mkGetClause :: (EnumKey k) => EntryDef k -> Q Clause
mkGetClause (EntryDef name key) = do
    let gqvName = mkName "getQV"
    bodyExp <- [e| $(conE name) <$> $(varE gqvName) |]
    let var = mkName "val"
        patterns = [ LitP $ toLit key ]
        body = NormalB $ bodyExp
    return $ Clause patterns body []


mkGetInvalidC :: Name -> Q Clause
mkGetInvalidC name = do
    body <- NormalB <$> [e| return $(conE name) |]
    return $ Clause [WildP] (body) []

-- }}}

-- }}}

