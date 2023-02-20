{-# LANGUAGE DeriveGeneric #-}

module DiaryViewer.Diary where

import Data.Aeson
import Data.Ix (Ix)
import qualified Data.Ix as Ix
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day)
import qualified Data.Time as Time
import GHC.Generics

data EntryHeading = EntryHeading
  { entryDay :: Day,
    entryTitle :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON EntryHeading

data Entry = Entry
  { entryHeading :: EntryHeading,
    entryContent :: Text
  }
  deriving (Eq, Show)

data TitleValidity
  = TitleEmpty
  | TitleValid
  | TitleTooLong
  deriving (Show)

data ContentValidity
  = ContentEmpty
  | ContentRegular
  | ContentLong
  | ContentTooLong
  deriving (Show)

type EntryValidity = (TitleValidity, ContentValidity)

titleValidity :: Text -> TitleValidity
titleValidity title
  | Text.null title = TitleEmpty
  | Text.length title <= 32 = TitleValid
  | otherwise = TitleTooLong

contentValidity :: Text -> ContentValidity
contentValidity content
  | Text.null content = ContentEmpty
  | Text.length content <= 512 = ContentRegular
  | Text.length content <= 1024 = ContentLong
  | otherwise = ContentTooLong

entryValidity :: Entry -> EntryValidity
entryValidity (Entry (EntryHeading _ title) content) =
  (titleValidity title, contentValidity content)

clashes :: [EntryHeading] -> [[EntryHeading]]
clashes =
  filter ((> 1) . length)
    . List.groupBy (\x y -> entryDay x == entryDay y)
    . List.sortOn entryDay

missingAmong :: (Ix a, Enum a) => [a] -> [a]
missingAmong l =
  case List.sort l of
    [] -> []
    x : xs -> concat . snd . List.foldl' f (x, []) $ xs
  where
    f (x1, hs) x2 = (x2, Ix.range (succ x1, pred x2) : hs)
