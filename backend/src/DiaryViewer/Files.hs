{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DiaryViewer.Files where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import qualified Data.Text as Text
import Data.Time (Day)
import qualified Data.Time as Time
import DiaryViewer.Diary
import qualified System.Directory as Dir
import qualified System.FSNotify as FSNotify
import qualified System.FilePath.Posix as FilePath
import qualified Data.Maybe as Maybe
import GHC.Generics
import Data.Aeson (ToJSON)

-- diaryPath :: IO FilePath
-- diaryPath = (<> "/diary") <$> Dir.getHomeDirectory

diaryPath :: FilePath
diaryPath = "diary"

entriesPaths :: IO [FilePath]
entriesPaths = Dir.listDirectory diaryPath

parseDay :: String -> Maybe Day
parseDay = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d"

parseHeading :: FilePath -> Maybe EntryHeading
parseHeading path = do
  let (dayStr, remainder) = splitAt 10 (FilePath.takeBaseName path)
  day <- parseDay dayStr
  case remainder of
    ' ' : title -> return $ EntryHeading day (Text.pack title)
    _ -> Nothing

parseHeadings :: [FilePath] -> [EntryHeading]
parseHeadings = Maybe.mapMaybe parseHeading

entryPath :: EntryHeading -> FilePath
entryPath (EntryHeading day title) = diaryPath <> "/" <> show day <> " " <> Text.unpack title <> ".txt"

readEntry :: EntryHeading -> IO Entry
readEntry heading = do
  str <- readFile (entryPath heading)
  return (Entry {entryHeading = heading, entryContent = Text.pack str})

writeEntry :: Entry -> IO ()
writeEntry (Entry heading content) = do
  writeFile (entryPath heading) (Text.unpack content)

readHeadings :: IO [EntryHeading]
readHeadings = parseHeadings <$> entriesPaths

data QueryDayFailure =
    QueryDayNotFound
    | QueryDayDuplicates [EntryHeading]
  deriving (Eq, Show, Generic)

instance ToJSON QueryDayFailure

queryDay :: Day -> IO (Either QueryDayFailure Entry)
queryDay day = do
    headings <- readHeadings
    case filter ((== day) . entryDay) headings of
        [heading] -> Right <$> readEntry heading
        [] -> return . Left $ QueryDayNotFound
        duplicates -> return . Left $ QueryDayDuplicates duplicates

data DiaryEvent =
  EntryUpdate EntryHeading
  | EntryRemoval EntryHeading
  deriving (Eq, Show, Generic)

instance ToJSON DiaryEvent

diaryEventFromFileEvent :: FSNotify.Event -> Maybe DiaryEvent
diaryEventFromFileEvent (FSNotify.Added path _ _) =
  EntryUpdate <$> parseHeading path
diaryEventFromFileEvent (FSNotify.Modified path _ _) =
  EntryUpdate <$> parseHeading path
diaryEventFromFileEvent (FSNotify.Removed path _ _) =
  EntryRemoval <$> parseHeading path
diaryEventFromFileEvent _ = Nothing

watchFileEvents :: (DiaryEvent -> IO ()) -> IO ()
watchFileEvents onFileEvent =
  void . FSNotify.withManager $ \manager -> do
    _ <-
      FSNotify.watchDir
        manager
        diaryPath
        (const True)
        (mapM_ onFileEvent . diaryEventFromFileEvent)
    forever $ threadDelay 10000
