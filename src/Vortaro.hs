{-# LANGUAGE OverloadedStrings #-}

module Vortaro where

import Data.List (isInfixOf)
import Data.Char (toLower)
import Data.Text (Text, split, pack, replace)
import Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Monoid

-- An Esperanto word, found on the LHS of a dictionary entry
data EoWord = EoWord Text deriving (Show, Eq)

-- A definition of an Esperanto word, found on the RHS of a dictionary entry
data Definition = Definition Text deriving (Show, Eq)

-- Product type representing a single line in the dictionary
data Entry = Entry EoWord Definition deriving (Show, Eq)

translate :: Text -> Text -> IO ()
translate word rawDic =
    if word == "" || translations == [] then TIO.putStrLn "No translation found"
    else do
        TIO.putStrLn ("\n\nThe word " <> word <> " has " <> (T.pack $  show . length $ translations) <> " possible definitions:")
        mapM_ (TIO.putStrLn . getOutputText) translations
            where translations = mkEntries (searchForWord (T.toLower word) (formatAllLines . getLines $ rawDic))

-- Return lines in the espdic for which the given Text (English or Esperanto Word) can be found
searchForWord :: Text -> [Text] -> [Text]
searchForWord w espdic = filter (T.isInfixOf word) espdic
  where word = if hasX w then getText (xReplace (EoWord w)) else w

definitionContainsWord :: Text -> Entry -> Bool
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

xReplaceHelper :: EoWord -> (Text, Text) -> EoWord
xReplaceHelper (EoWord word) (x, d) = (EoWord (T.replace x d word))

xReplace :: EoWord -> EoWord
xReplace word = foldl xReplaceHelper word xMap
  where xMap = [("cx", "ĉ"), ("cX", "ĉ"), ("Cx", "Ĉ"), ("CX", "Ĉ"),
                ("gx", "ĝ"), ("gX", "ĝ"), ("Gx", "Ĝ"), ("GX", "Ĝ"),
                ("hx", "ĥ"), ("hX", "ĥ"), ("Hx", "Ĥ"), ("HX", "Ĥ"),
                ("jx", "ĵ"), ("jX", "ĵ"), ("Jx", "Ĵ"), ("JX", "Ĵ"),
                ("sx", "ŝ"), ("sX", "ŝ"), ("Sx", "Ŝ"), ("SX", "Ŝ"),
                ("ux", "ŭ"), ("uX", "ŭ"), ("Ux", "Ŭ"), ("UX", "Ŭ")]

hasX :: Text -> Bool
hasX word = T.isInfixOf "x" word || T.isInfixOf "X" word

getText :: EoWord -> Text
getText (EoWord word) = word
