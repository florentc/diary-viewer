module DiaryViewer.Api where

{-
  
* State

diaryPath :: FilePath

* Actions

getDiaryPath :: m FilePath
setDiaryPath :: FilePath -> m ()

getEntries :: m (Either err [EntryHeading])
readEntry :: EntryHeading -> m (Either err Entry)
editEntry :: Entry -> m (Maybe err)
entryValidity :: Entry -> m EntryValidity

checkDateClashes :: [EntryHeading] -> m [EntryHeading]
checkHoles :: Day -> [EntryHeading] -> m [Day]
               |
             today

* Events

updatedEntry
deletedEntry

-}
