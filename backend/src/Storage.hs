{-# LANGUAGE OverloadedStrings #-}

module Storage
  ( loadBook
  , storeBook
  , removeBook
  , addFailedIsbn
  , getFailedIsbns
  , listAllIsbns
  , listAllBooks
  , storeCover
  , loadCover
  ) where

import           Book
import           Control.Monad
import qualified Data.ByteString     as BS
import           Data.Either
import           Data.Maybe
import qualified Data.Set            as Set
import qualified Data.Text           as T
import           Data.Time.Clock
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

removeBook :: FilePath -> Isbn -> IO ()
removeBook directory isbn = do
  jsonExists <- doesFileExist jsonPath
  if jsonExists then
    removeFile jsonPath
  else
     pure ()

  imgExists <- doesFileExist imgPath
  if imgExists then
    removeFile imgPath
  else
    pure ()
  where
    jsonPath = _isbnToPath directory ".json" isbn
    imgPath = _isbnToPath directory ".img" isbn

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

addFailedIsbn :: FilePath -> Isbn  -> IO ()
addFailedIsbn directory isbn = do
  let path = directory ++ "/" ++ "failed_isbns.txt"
  time <- getCurrentTime
  appendFile path $ "\n" ++ show isbn ++ "\t" ++ show time

_getRight :: Either a b -> Maybe b
_getRight (Left  _) = Nothing
_getRight (Right v) = Just v

getFailedIsbns :: FilePath -> IO [Isbn]
getFailedIsbns directory = do
  let path = directory ++ "/" ++ "failed_isbns.txt"
  entries <- filter (/= "") . lines <$> readFile path

  let isbnStrings = distinct $ map ((!! 0) . words) entries
  return $ mapMaybe (_getRight . isbnFromString) isbnStrings

  where
    distinct :: (Ord a) => [a] -> [a]
    distinct = Set.toList . Set.fromList

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

loadCover :: FilePath -> Isbn -> IO (Maybe BS.ByteString)
loadCover directory isbn = do
  fileExists <- doesFileExist filePath

  if fileExists then do
    val <- BS.readFile filePath
    return $ Just val
  else
    return Nothing
  where
    filePath = _isbnToPath directory ".img" isbn
