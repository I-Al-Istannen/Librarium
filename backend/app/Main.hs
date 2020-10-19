module Main where

import           Book
import           Control.Concurrent
import           Control.Monad
import qualified Data.Map                 as Map
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           Storage
import           System.Environment
import           Webserver.Server

main :: IO ()
main = do
  arguments <- getArgs
  guard $ length arguments == 3
  let dataDir = head arguments
  let user = (arguments !! 1, arguments !! 2)

  initialBooks <- newEmptyMVar
  let config = ServerConfig {
     serverDataDir = dataDir,
     serverBooks = initialBooks,
     serverUsers = [user]
  }

  allBooks <- listAllBooks $ serverDataDir config
  let bookMap = map (\b -> (_isbn b, b)) allBooks
  putMVar initialBooks $ Map.fromList bookMap

  withStdoutLogger $ \logger ->
    runSettings (setPort 8081 $ setLogger logger $ defaultSettings) (app config)
