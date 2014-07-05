-- Module containing functions and data types for parsing lines from IRC.
module ChatCore.IRC.Line
    ( IRCLine
    , IRCSource
    , IRCCommand

    , parseLine

    , lineParserTests
    ) where

import Control.Monad
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Text.Parsec
import Text.Parsec.ByteString
import Test.HUnit

-- {{{ Interface and data types

-- | Represents a line's source string.
type IRCSource = T.Text

-- | Represents a single argument.
type IRCArgument = T.Text

-- | Represents the body of an IRC line.
type IRCBody = T.Text

-- | Represents the 'command' part of an IRC line.
data IRCCommand = IRCCommand T.Text deriving (Show, Eq)


-- | Represents a line in the IRC protocol.
data IRCLine = IRCLine
    { ilSourceStr   :: Maybe IRCSource
    , ilCommand     :: IRCCommand
    , ilArgs        :: [IRCArgument]
    , ilBody        :: Maybe IRCBody
    } deriving (Show, Eq)

-- | Parses the given text to an IRC line structure.
parseLine :: B.ByteString -> Maybe IRCLine
parseLine str = Nothing

-- }}}


-- {{{ Parsers

-- {{{ Tests

assertParsed :: (Eq a) => String -> B.ByteString -> Parser a -> a -> Assertion
assertParsed failMsg testData parser expected = do
    either (\_   -> assertFailure failMsg)
           (\val -> unless (val == expected) (assertFailure failMsg))
           result
  where result = parse parser "" testData

lineParserTests = TestList [lineParserTest1, lineParserTest2, lineParserTest3, lineParserTest4]

lineParserTest1 = TestCase (assertParsed "parse line with arguments, source, and body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel :test body content"
    lineParser
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = IRCCommand "PRIVMSG"
        , ilArgs        = ["#channel"]
        , ilBody        = Just "test body content"
        }
    )

lineParserTest2 = TestCase (assertParsed "parse line with arguments and body, but no source"
    "PRIVMSG #channel :test body content"
    lineParser
    IRCLine
        { ilSourceStr   = Nothing
        , ilCommand     = IRCCommand "PRIVMSG"
        , ilArgs        = ["#channel"]
        , ilBody        = Just "test body content"
        }
    )

lineParserTest3 = TestCase (assertParsed "parse line with source and body, but no arguments"
    ":Forkk!~forkk@awesome/forkk PRIVMSG :test body content"
    lineParser
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = IRCCommand "PRIVMSG"
        , ilArgs        = []
        , ilBody        = Just "test body content"
        }
    )

lineParserTest4 = TestCase (assertParsed "parse line with source and arguments, but no body"
    ":Forkk!~forkk@awesome/forkk PRIVMSG #channel"
    lineParser
    IRCLine
        { ilSourceStr   = Just "Forkk!~forkk@awesome/forkk"
        , ilCommand     = IRCCommand "PRIVMSG"
        , ilArgs        = ["#channel"]
        , ilBody        = Nothing
        }
    )

-- }}}

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
segment = T.pack <$> segChars
  where
    segChars = do
        -- Read a character.
        c <- anyChar
        if c == ' '
           -- If it's a space, we're done.
           then return []
           -- Otherwise, parse the rest of the segment.
           else (c:) <$> (segChars <|> (eof >> return []))

-- | Parser which parses the source string at the beginning of a line.
-- Will fail and consume no input if no source string is present.
sourceString :: Parser IRCSource
sourceString = do
    -- If the line starts with a colon, read the source string.
    char ':'
    segment

-- | Parser which parses a command.
command :: Parser IRCCommand
command = IRCCommand <$> segment

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

