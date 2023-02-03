{-# LANGUAGE OverloadedStrings #-}

module DiaryViewer.Files where

import Data.Maybe (fromJust)
import qualified Data.Text as Text
import Data.Time (Day)
import qualified Data.Time as Time
import DiaryViewer.Diary
import qualified System.Directory as Dir
import qualified System.FilePath.Posix as FilePath

-- diaryPath :: IO FilePath
-- diaryPath = (<> "/diary") <$> Dir.getHomeDirectory

diaryPath :: FilePath
diaryPath = "/home/private/diary"

entriesPaths :: IO [FilePath]
entriesPaths = Dir.listDirectory diaryPath

parseDay :: String -> Maybe Day
parseDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

parseHeading :: String -> Maybe EntryHeading
parseHeading str = do
  let (dayStr, remainder) = splitAt 10 str
  day <- parseDay dayStr
  case remainder of
    ' ' : title -> return $ EntryHeading day (Text.pack title)
    _ -> Nothing

parseHeadings :: [FilePath] -> Maybe [EntryHeading]
parseHeadings = mapM (parseHeading . FilePath.takeBaseName)

entryPath :: EntryHeading -> FilePath
entryPath (EntryHeading day title) = diaryPath <> "/" <> show day <> " " <> Text.unpack title <> ".txt"

readEntry :: EntryHeading -> IO Entry
readEntry heading = do
  str <- readFile (entryPath heading)
  return (Entry {entryHeading = heading, entryContent = Text.pack str})

writeEntry :: Entry -> IO ()
writeEntry (Entry heading content) = do
  writeFile (entryPath heading) (Text.unpack content)

readHeadings :: IO (Maybe [EntryHeading])
readHeadings = parseHeadings <$> entriesPaths
