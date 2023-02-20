module DiaryViewer.Server where

import Servant
import DiaryViewer.Api
import DiaryViewer.Diary
import DiaryViewer.Files
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class (liftIO)

server :: Server ApiFoo
server =
  return diaryPath
  :<|> liftIO readHeadings
  :<|> (clashes <$> liftIO readHeadings)
  :<|> (missingAmong . map entryDay <$> liftIO readHeadings)

apiFoo :: Proxy ApiFoo
apiFoo = Proxy

app :: Application
app = serve apiFoo server

main :: IO ()
main = run 8001 app 
