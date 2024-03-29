{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DiaryViewer.Api where

import Data.Time (Day)
import DiaryViewer.Diary
import DiaryViewer.Files
import Servant
import Servant.API.WebSocket

type Api =
  "diaryPath" :> Get '[JSON] FilePath
    :<|> "entries" :> Get '[JSON] [EntryHeading]
    :<|> "clashes" :> Get '[JSON] [[EntryHeading]]
    :<|> "missing" :> Get '[JSON] [Day]
    :<|> "diary" :> Get '[JSON] Diary
    :<|> "entry" :> Capture "day" Day :> Get '[JSON] (Either QueryDayFailure Entry)
    :<|> "entry" :> ReqBody '[JSON] Entry :> Post '[JSON] ()
    :<|> "updates" :> WebSocket
