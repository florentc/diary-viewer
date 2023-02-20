{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DiaryViewer.Api where

import Data.Time (Day)
import DiaryViewer.Diary
import Servant

type Api =
  "diaryPath" :> Get '[JSON] FilePath
    :<|> "diaryPath" :> ReqBody '[JSON] FilePath :> Post '[JSON] (Either String FilePath)
    :<|> "entry" :> QueryParam "day" Day :> Get '[JSON] (Either String Entry)
    :<|> "entry" :> QueryParam "day" Day :> ReqBody '[JSON] Entry :> Post '[JSON] (Either String Entry)
    :<|> "entries" :> Get '[JSON] (Either String [EntryHeading])
    :<|> "clashes" :> Get '[JSON] (Either String [EntryHeading])
    :<|> "holes" :> Get '[JSON] (Either String [EntryHeading])

type ApiFoo =
  "diaryPath" :> Get '[JSON] FilePath
  :<|> "entries" :> Get '[JSON] [EntryHeading]
  :<|> "clashes" :> Get '[JSON] [[EntryHeading]]
  :<|> "missing" :> Get '[JSON] [Day]

-- Events:
-- updatedEntry
-- deletedEntry
