{-# LANGUAGE OverloadedStrings #-}

module Vortaro where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Text (Text, split, pack)
import Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Monoid

-- An English word
type EnWord = Text

-- An Esperanto word, found on the LHS of a dictionary entry
data EoWord = EoWord Text deriving (Show, Eq)

-- A definition of an Esperanto word, found on the RHS of a dictionary entry
data Definition = Definition Text deriving (Show, Eq)

-- Product type representing a single line in the dictionary
data Entry = Entry EoWord Definition deriving (Show, Eq)

translate :: EnWord -> Text -> IO ()
translate word rawDic =
    if word == "" || translations == [] then TIO.putStrLn "No translation found"
    else do
        TIO.putStrLn ("\n\nThe word " <> word <> " has " <> (T.pack $  show . length $ translations) <> " possible definitions:")
        mapM_ (TIO.putStrLn . getOutputText) translations
            where translations = mkEntries (searchForWord (T.toLower word) (formatAllLines . getLines $ rawDic))

-- Return lines in the espdic for which the given EnWord can be found
searchForWord :: EnWord -> [Text] -> [Text]
searchForWord word espdic = filter (T.isInfixOf word) espdic

definitionContainsWord :: EnWord -> Entry -> Bool
definitionContainsWord word (Entry _ (Definition def)) = T.isInfixOf word def

-- Format the EspDic input Text prior to its transformation into the real type
format :: Text -> Text
format = T.toLower . stripQuotes

formatAllLines :: [Text] -> [Text]
formatAllLines = map format

-- Get all lines in the Esperanto dictionary, skipping the first because it's the header
getLines :: Text -> [Text]
getLines = drop 1 . T.lines

-- Dictionary entries are split by a :
-- Construct an Entry for a given line
mkEntry :: Text -> Entry
mkEntry line = (Entry (EoWord left) (Definition right))
    where left = parts !! 0
          right = parts !! 1
          parts = split (==':') line

mkEntries :: [Text] -> [Entry]
mkEntries = map mkEntry

stripQuotes :: Text -> Text
stripQuotes = T.filter (/='"')

getOutputText :: Entry -> Text
getOutputText (Entry (EoWord word) (Definition def)) = format word <> " => " <> format def
