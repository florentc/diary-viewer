module DiaryViewer.Diary where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day)
import qualified Data.List as List

data EntryHeading = EntryHeading
  { entryDay :: Day,
    entryTitle :: Text
  }
  deriving (Eq, Show)

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

sortHeadings :: [EntryHeading] -> [EntryHeading]
sortHeadings = List.sortOn entryDay
