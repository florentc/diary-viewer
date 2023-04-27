module Main where

import Network.Wai.Handler.Warp
import DiaryViewer.Server
import qualified Servant.Server

main :: IO ()
main = run 8001 app 
