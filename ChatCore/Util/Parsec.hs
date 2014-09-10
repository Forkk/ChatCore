module ChatCore.Util.Parsec where

import Control.Applicative ((<$>))
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.ByteString

-- | Reads characters into a Text until the given character is read.
charsUntil :: Char -> Parser T.Text
charsUntil stop = T.pack <$> charsUntilStr stop

-- | Reads characters into a String until the given character is read.
charsUntilStr :: Char -> Parser String
charsUntilStr stop = segChars
  where
    segChars = do
        -- Read a character.
        c <- anyChar
        if c == stop
           -- If it's our ending char, we're done.
           then return []
           -- Otherwise, parse the rest of the segment.
           else (c:) <$> (segChars <|> (eof >> return []))


