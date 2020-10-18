{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( loadBook
  , storeBook
  , listAllIsbns
  , listAllBooks
  , storeCover
  ) where

import           Book
import           Control.Monad
import qualified Data.ByteString     as BS
import           Data.Either
import           Data.Maybe
import qualified Data.Text           as T
import           Network.HTTP.Simple
import           System.Directory

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

listAllIsbns :: FilePath -> IO [Isbn]
listAllIsbns directory = do
  contents <- getDirectoryContents directory

  let isbnFiles = filter (T.isSuffixOf ".json" . T.pack) contents
  let isbnNames = mapMaybe (T.stripSuffix ".json" . T.pack) isbnFiles
  let isbns = rights $ map (isbnFromString  . T.unpack) isbnNames

  return isbns

listAllBooks :: FilePath -> IO [Book]
listAllBooks directory = do
  isbns <- listAllIsbns directory

  books <- mapM (loadBook directory) isbns

  return $ rights books

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
