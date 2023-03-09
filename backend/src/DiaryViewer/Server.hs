module DiaryViewer.Server where

import Servant
import DiaryViewer.Api
import DiaryViewer.Diary
import DiaryViewer.Files
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)

server :: Server Api
server =
  return diaryPath
  :<|> liftIO readHeadings
  :<|> (clashes <$> liftIO readHeadings)
  :<|> (missingAmong . map entryDay <$> liftIO readHeadings)
  :<|> liftIO . queryDay
  :<|> liftIO . writeEntry

api :: Proxy Api
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 8001 app 
