{-# LANGUAGE OverloadedStrings #-}

module DiaryViewer.Server where

import Control.Monad.IO.Class (MonadIO, liftIO)
import DiaryViewer.Api
import DiaryViewer.Diary
import DiaryViewer.Files
import Network.Wai.Middleware.Cors (simpleCors)
import qualified Network.WebSockets as WebSockets
import Servant
import qualified Data.Aeson as Aeson

server :: Server Api
server =
  return diaryPath
    :<|> liftIO readHeadings
    :<|> (clashes <$> liftIO readHeadings)
    :<|> (missingAmong . map entryDay <$> liftIO readHeadings)
    :<|> do
      headings <- liftIO readHeadings
      return
        ( Diary
            headings
            (clashes headings)
            (missingAmong . map entryDay $ headings)
        )
    :<|> liftIO . queryDay
    :<|> liftIO . writeEntry
    :<|> streamUpdates

streamUpdates :: MonadIO m => WebSockets.Connection -> m ()
streamUpdates c =
  liftIO $
    WebSockets.withPingThread
      c
      10
      (return ())
      (watchFileEvents (WebSockets.sendTextData c . Aeson.encode . Aeson.toJSON))

api :: Proxy Api
api = Proxy

app :: Application
app = simpleCors (serve api server)
