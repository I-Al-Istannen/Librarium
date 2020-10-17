{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( loadBook
  , storeBook
  , storeCover
  ) where

import           Book
import           Control.Monad
import qualified Data.ByteString     as BS
import qualified Data.Text           as T
import           Network.HTTP.Simple

type Url = String

_isbnToPath :: FilePath -> Isbn -> FilePath
_isbnToPath directory isbn = let
    fileName = show (toIsbn13 isbn) ++ ".json"
    filePath = directory ++ "/" ++ fileName
  in filePath

loadBook :: FilePath -> Isbn -> IO (Either String Book)
loadBook directory isbn = do
  fileContent <- readFile $ _isbnToPath directory isbn
  return $ jsonToBook (T.pack fileContent)

storeBook :: FilePath -> Book -> IO ()
storeBook directory book = do
  writeFile filePath $ T.unpack $ bookToJson book
  return ()
  where
    filePath = _isbnToPath directory (_isbn book)

storeCover :: FilePath -> Url -> Isbn -> IO ()
storeCover directory url isbn = do
  request <- parseRequest url
  response <- httpBS request

  guard $ getResponseStatusCode response == 200

  let body = getResponseBody response

  BS.writeFile filePath body

  return ()
  where
    filePath = _isbnToPath directory isbn
