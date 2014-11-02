{-# LANGUAGE TemplateHaskell #-}
-- Module containing functions and data types for parsing lines from IRC.
module ChatCore.IRC.Line
    ( IRCLine (..)
    , IRCCommand (..)

    , IRCSource (..)
    , srcUser

    , IRCUser (..)
    , iuNick
    , iuIdent
    , iuHost

    , parseLine
    , lineToByteString

    , lineParserTests
    ) where

import Control.Monad
import Control.Applicative hiding ((<|>))
import Control.Lens
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.SafeCopy
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import Text.Parsec
import Text.Parsec.ByteString
import Test.HUnit

import ChatCore.IRC.Commands
import ChatCore.Util.Parsec

-- {{{ Interface and data types

data IRCUser = IRCUser
    { _iuNick  :: T.Text
    , _iuIdent :: T.Text
    , _iuHost  :: T.Text
    } deriving (Show, Read, Eq)
$(makeLenses ''IRCUser)
$(deriveSafeCopy 0 'base ''IRCUser)

-- | Represents the source that a message came from.
data IRCSource
    = ServerSource T.Text
    | UserSource { _srcUser :: IRCUser }
    deriving (Show, Read, Eq)
$(makeLenses ''IRCSource)
$(deriveSafeCopy 0 'base ''IRCSource)

-- | Represents a single argument.
type IRCArgument = T.Text

-- | Represents the body of an IRC line.
type IRCBody = T.Text


-- | Represents a line in the IRC protocol.
data IRCLine = IRCLine
    { ilSource      :: Maybe IRCSource
    , ilCommand     :: IRCCommand
    , ilArgs        :: [IRCArgument]
    , ilBody        :: Maybe IRCBody
    } deriving (Show, Eq)

-- | Parses the given text to an IRC line structure.
parseLine :: B.ByteString -> Either ParseError IRCLine
parseLine str = parse lineParser "" str

-- }}}

-- {{{ Parsers

-- | Parsec parser for IRC lines.
lineParser :: Parser IRCLine
lineParser = do
    -- Read the source string if it is present.
    lineSource <- (Just <$> source) <|> return Nothing

    -- Next, read the command.
    lineCmd <- command

    -- Read all of the arguments until we reach the body or the end.
    lineArgs <- manyTill arg (((try $ lookAhead $ body) >> return ()) <|> eof)

    -- Read the body.
    lineBody <- (Just <$> body) <|> return Nothing

    return IRCLine
        { ilSource = lineSource
        , ilCommand = lineCmd
        , ilArgs = lineArgs
        , ilBody = lineBody
        }

-- | Parses one segment of an IRC line.
-- A segment is one of the line's space separated tokens.
segment :: Parser T.Text
segment = charsUntil ' '

-- | Parser which parses the source at the beginning of a line.
-- Will fail and consume no input if no source string is present.
source :: Parser IRCSource
source = do
    -- If the line starts with a colon, read the source string.
    char ':'
    src <- try userSource <|> serverSource
    return src

-- | Parser which parses a user source in the format <nick>!<ident>@<host>.
userSource :: Parser IRCSource
userSource = do
    let alphanumeric = ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']
    nick <- manyTill (oneOf (alphanumeric <> "-_.{}[]|")) (char '!')
    ident <- manyTill (oneOf (alphanumeric <> "~-_.{}[]")) (char '@')
    host <- manyTill (oneOf (alphanumeric <> "-_:./")) (char ' ')
    return $ UserSource $ IRCUser (T.pack nick) (T.pack ident) (T.pack host)

serverSource :: Parser IRCSource
serverSource = ServerSource <$> charsUntil ' '


-- | Parser which parses a command.
command :: Parser IRCCommand
command = icmdFromStr <$> charsUntilStr ' '

-- | Parser which parses a single argument.
arg :: Parser IRCArgument
arg = segment

-- | Parser which parses the body of an IRC line.
body :: Parser IRCBody
body = do
    -- Read the colon.
    char ':'
    -- Read the body until the end of the line.
    T.pack <$> manyTill anyChar eof

-- }}}

-- {{{ To ByteString

-- | Serialize the given line structure to a strict bytestring.
lineToByteString :: IRCLine -> B.ByteString
lineToByteString line = BL.toStrict $ TL.encodeUtf8 $ TL.toLazyText $ lineTextBuilder line

-- FIXME: Don't allow newlines anywhere in the line.
lineTextBuilder :: IRCLine -> TL.Builder
lineTextBuilder line =
       maybe mempty (\t -> TL.singleton ':' <> t <> TL.singleton ' ') (sourceStr <$> ilSource line)
    <> (TL.fromString $ icmdToStr $ ilCommand line)
    <> (mconcat $ map spaceAndText $ ilArgs line)
    <> maybe mempty (\t -> TL.fromText " :" <> TL.fromText t) (ilBody line)
  where
    spaceAndText text = TL.singleton ' ' <> TL.fromText text
    sourceStr (UserSource (IRCUser nick ident host)) =
           TL.fromText nick
        <> TL.singleton '!' <> TL.fromText ident
        <> TL.singleton '@' <> TL.fromText host
    sourceStr (ServerSource host) = TL.fromText host

-- }}}

-- {{{ Tests

assertParsed :: (Eq a, Show a) => String -> B.ByteString -> Parser a -> a -> Assertion
assertParsed name testData parser expected = do
    either (assertFailure . parseErrMsg)
           (\val -> unless (val == expected) (assertFailure $ wrongValMsg val))
           result
  where
    result = parse parser "" testData
    parseErrMsg err = "'" ++ name ++ "' failed to parse with the following error:\n" ++ show err
    wrongValMsg val = "'" ++ name ++ "' parsed to an unexpected value:\n" ++ show val


ircLineTestCase :: String -> B.ByteString -> IRCLine -> Test
ircLineTestCase msg str line = TestList
    [ TestCase (assertParsed msg str lineParser line)
    , TestCase (assertEqual (msg ++ " line to text") (lineToByteString line) str)
    ]

lineParserTests :: Test
lineParserTests = TestList
    [ lineParserTest1
    , lineParserTest2
    , lineParserTest3
    , lineParserTest4
    , lineParserTest5
    ]

lineParserTest1 :: Test
lineParserTest1 = ircLineTestCase "parse line with arguments, source, and body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel :test body content"
    IRCLine
        { ilSource  = Just $ UserSource $ IRCUser "Forkk" "~forkk" "awesome/forkk"
        , ilCommand = ICmdPrivmsg
        , ilArgs    = ["#channel"]
        , ilBody    = Just "test body content"
        }

lineParserTest2 :: Test
lineParserTest2 = ircLineTestCase "parse line with arguments, network source, and body"
    ":server.network.net PRIVMSG #channel :test body content"
    IRCLine
        { ilSource  = Just $ UserSource $ IRCUser "Forkk" "~forkk" "awesome/forkk"
        , ilCommand = ICmdPrivmsg
        , ilArgs    = ["#channel"]
        , ilBody    = Just "test body content"
        }

lineParserTest3 :: Test
lineParserTest3 = ircLineTestCase "parse line with arguments and body, but no source"
    "PRIVMSG #channel :test body content"
    IRCLine
        { ilSource  = Nothing
        , ilCommand = ICmdPrivmsg
        , ilArgs    = ["#channel"]
        , ilBody    = Just "test body content"
        }

lineParserTest4 :: Test
lineParserTest4 = ircLineTestCase "parse line with source and body, but no arguments"
    ":Forkk!~forkk@awesome/forkk PRIVMSG :test body content"
    IRCLine
        { ilSource  = Just $ UserSource $ IRCUser "Forkk" "~forkk" "awesome/forkk"
        , ilCommand = ICmdPrivmsg
        , ilArgs    = []
        , ilBody    = Just "test body content"
        }

lineParserTest5 :: Test
lineParserTest5 = ircLineTestCase "parse line with source and arguments, but no body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel"
    IRCLine
        { ilSource  = Just $ UserSource $ IRCUser "Forkk" "~forkk" "awesome/forkk"
        , ilCommand = ICmdPrivmsg
        , ilArgs    = ["#channel"]
        , ilBody    = Nothing
        }

-- }}}

