{-# LANGUAGE TemplateHaskell, QuasiQuotes, TupleSections #-}
-- | Module for the IRC library's template Haskell stuff.
module ChatCore.IRC.TH where

import Control.Applicative
import Data.Maybe
import Language.Haskell.TH

-- This is an abomination, but it makes defining IRC commands much easier.

-- IRC command definition TH function.
defineIRCCmds :: [(String, String)] -> Q [Dec]
defineIRCCmds strCmds = do
    nameCmds <- mapM (\(n, s) -> newName n >>= return . (, s)) strCmds
    dt <- dataType nameCmds
    ts <- toStr    nameCmds
    fs <- fromStr  nameCmds
    return [dt, ts, fs]

  where
    otherCmdName = mkName "ICmdOther"
    dataTypeName = mkName "IRCCommand"
    toStrName = mkName "icmdToStr"
    fromStrName = mkName "icmdFromStr"
    
    -- Defines the IRC command data structure.
    dataType cmds = do
        ocmd <- otherCmdCons
        showName <- fromJust <$> lookupTypeName "Prelude.Show"
        eqName <- fromJust <$> lookupTypeName "Prelude.Eq"
        let cons = (ocmd : map dataTypeCons cmds)
        return $ DataD [] dataTypeName [] cons [showName, eqName]

    dataTypeCons (cmd, _) = NormalC cmd []

    otherCmdCons = do
        textType <- fromJust <$> lookupTypeName "Data.Text.Text"
        return $ NormalC otherCmdName [(IsStrict, ConT textType)]


    -- Defines the function to convert IRC commands to strings.
    toStr cmds = do
        clauses <- sequence (otherToStrClause : map toStrClause cmds)
        return $ FunD toStrName clauses

    toStrClause (cmd, str) =
        return $ Clause [ConP cmd []] (NormalB $ LitE $ StringL str) []

    otherToStrClause = do
        cstrVar <- newName "str"
        return $ Clause [ConP otherCmdName [VarP cstrVar]] (NormalB $ VarE cstrVar) []


    -- Defines the function to convert strings to IRC commands.
    fromStr cmds = do
        clauses <- sequence (map fromStrClause cmds ++ [otherFromStrClause])
        return $ FunD fromStrName clauses

    fromStrClause (cmd, str) =
        return $ Clause [LitP $ StringL str] (NormalB $ ConE cmd) []

    otherFromStrClause = do
        cstrVar <- newName "str"
        return $ Clause [VarP cstrVar] (NormalB $ AppE (ConE otherCmdName) (VarE cstrVar)) []
    


