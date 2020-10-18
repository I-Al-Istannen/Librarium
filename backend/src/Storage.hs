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

_isbnToPath :: FilePath -> String -> Isbn -> FilePath
_isbnToPath directory suffix isbn = let
    fileName = show (toIsbn13 isbn) ++ suffix
    filePath = directory ++ "/" ++ fileName
  in filePath

loadBook :: FilePath -> Isbn -> IO (Either String Book)
loadBook directory isbn = do
  fileContent <- readFile $ _isbnToPath directory ".json" isbn
  return $ jsonToBook (T.pack fileContent)

storeBook :: FilePath -> Book -> IO ()
storeBook directory book = do
  writeFile filePath $ T.unpack $ bookToJson book
  return ()
  where
    filePath = _isbnToPath directory ".json" (_isbn book)

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

storeCover :: FilePath -> Isbn -> Url -> IO ()
storeCover directory isbn url = do
  request <- parseRequest url
  response <- httpBS request

  guard $ getResponseStatusCode response == 200

  let body = getResponseBody response

  BS.writeFile filePath body

  return ()
  where
    filePath = _isbnToPath directory ".img" isbn
