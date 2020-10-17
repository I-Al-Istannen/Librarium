module Storage
  ( loadBook
  , storeBook
  ) where

import           Book
import qualified Data.Text       as T

loadBook :: FilePath -> Isbn -> IO (Either String Book)
loadBook directory isbn = do
  fileContent <- readFile filePath
  return $ jsonToBook (T.pack fileContent)
  where
    fileName = show (toIsbn13 isbn) ++ ".json"
    filePath = directory ++ "/" ++ fileName

storeBook :: FilePath -> Book -> IO ()
storeBook directory book = do
  writeFile filePath $ T.unpack $ bookToJson book
  return ()
  where
    fileName = show (toIsbn13 (_isbn book)) ++ ".json"
    filePath = directory ++ "/" ++ fileName
