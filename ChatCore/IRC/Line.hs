{-# LANGUAGE TemplateHaskell #-}
-- Module containing functions and data types for parsing lines from IRC.
module ChatCore.IRC.Line
    ( IRCLine (IRCLine)
    , IRCSource
    , IRCCommand (..)

    , ilSourceStr
    , ilCommand
    , ilArgs
    , ilBody

    , parseLine
    , lineToByteString

    , lineParserTests
    ) where

import Control.Monad
import Control.Applicative hiding ((<|>))
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Typeable
import Text.Parsec
import Text.Parsec.ByteString
import Test.HUnit

import ChatCore.IRC.Commands
import ChatCore.Util.Parsec

-- {{{ Interface and data types

-- | Represents a line's source string.
type IRCSource = T.Text

-- | Represents a single argument.
type IRCArgument = T.Text

-- | Represents the body of an IRC line.
type IRCBody = T.Text


-- | Represents a line in the IRC protocol.
data IRCLine = IRCLine
    { ilSourceStr   :: Maybe IRCSource
    , ilCommand     :: IRCCommand
    , ilArgs        :: [IRCArgument]
    , ilBody        :: Maybe IRCBody
    } deriving (Show, Eq, Typeable)

-- | Parses the given text to an IRC line structure.
parseLine :: B.ByteString -> Either ParseError IRCLine
parseLine str = parse lineParser "" str

-- }}}

-- {{{ Parsers

-- | Parsec parser for IRC lines.
lineParser :: Parser IRCLine
lineParser = do
    -- Read the source string if it is present.
    lineSourceStr <- (Just <$> sourceString) <|> return Nothing

    -- Next, read the command.
    lineCmd <- command

    -- Read all of the arguments until we reach the body or the end.
    lineArgs <- manyTill argument (((try $ lookAhead $ body) >> return ()) <|> eof)

    -- Read the body.
    lineBody <- (Just <$> body) <|> return Nothing

    return IRCLine
        { ilSourceStr = lineSourceStr
        , ilCommand = lineCmd
        , ilArgs = lineArgs
        , ilBody = lineBody
        }

-- | Parses one segment of an IRC line.
-- A segment is one of the line's space separated tokens.
segment :: Parser T.Text
segment = charsUntil ' '

-- | Parser which parses the source string at the beginning of a line.
-- Will fail and consume no input if no source string is present.
sourceString :: Parser IRCSource
sourceString = do
    -- If the line starts with a colon, read the source string.
    char ':'
    segment

-- | Parser which parses a command.
command :: Parser IRCCommand
command = icmdFromStr <$> charsUntilStr ' '

-- | Parser which parses a single argument.
argument :: Parser IRCArgument
argument = segment

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
       maybe mempty (\t -> TL.singleton ':' <> textAndSpace t) (ilSourceStr line)
    <> (TL.fromString $ icmdToStr $ ilCommand line)
    <> (mconcat $ map spaceAndText $ ilArgs line)
    <> maybe mempty (\t -> TL.fromText " :" <> TL.fromText t) (ilBody line)
  where
    textAndSpace text = TL.fromText text <> TL.singleton ' '
    spaceAndText text = TL.singleton ' ' <> TL.fromText text

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
    ]

lineParserTest1 :: Test
lineParserTest1 = ircLineTestCase "parse line with arguments, source, and body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel :test body content"
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = ICmdPrivmsg
        , ilArgs        = ["#channel"]
        , ilBody        = Just "test body content"
        }

lineParserTest2 :: Test
lineParserTest2 = ircLineTestCase "parse line with arguments and body, but no source"
    "PRIVMSG #channel :test body content"
    IRCLine
        { ilSourceStr   = Nothing
        , ilCommand     = ICmdPrivmsg
        , ilArgs        = ["#channel"]
        , ilBody        = Just "test body content"
        }

lineParserTest3 :: Test
lineParserTest3 = ircLineTestCase "parse line with source and body, but no arguments"
    ":Forkk!~forkk@awesome/forkk PRIVMSG :test body content"
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = ICmdPrivmsg
        , ilArgs        = []
        , ilBody        = Just "test body content"
        }

lineParserTest4 :: Test
lineParserTest4 = ircLineTestCase "parse line with source and arguments, but no body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel"
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = ICmdPrivmsg
        , ilArgs        = ["#channel"]
        , ilBody        = Nothing
        }

-- }}}

