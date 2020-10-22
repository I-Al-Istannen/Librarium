{-# LANGUAGE OverloadedStrings #-}
module Scraping.BuchhandelScraper
  ( scrapeBuchhandel
  )
  where

-- https://openlibrary.org/isbn/3442067375.json

import           Book
import           Control.Applicative
import           Control.Monad             (guard)
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as A
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.List.NonEmpty        as NE
import qualified Data.Text                 as T
import           Network.HTTP.Simple
import           Scraping.GoodreadsScraper (CrawlResult (..))

_resultFromValue :: Isbn -> A.Object -> A.Parser CrawlResult
_resultFromValue isbn val = do
  attributes <- val A..: "attributes" :: A.Parser A.Object
  title <- attributes A..: "title" :: A.Parser T.Text
  pages <- attributes A..:? "numPages" :: A.Parser (Maybe Int)
  description <- ((NE.head <$> attributes A..: "mainDescriptions") >>= (A..: "description")) <|> pure Nothing
  coverUrl <- attributes A..:? "coverUrl" :: A.Parser (Maybe String)
  language <- NE.head <$> attributes A..: "mainLanguages"
  author <- (attributes A..: "contributors" >>= traverse (A..: "name")) <|> pure []

  let book = Book{
    _isbn      = isbn,
    _title     = title,
    _author    = map correctAuthorNameOrder author,
    _pages     = pages,
    _location  = Nothing,
    _summary   = description,
    _language  = language,
    _crawledBy = "buchhandel.de"
  }
  return $ CrawlResult {_book=book, _coverImage=coverUrl}
  where
    correctAuthorNameOrder :: T.Text -> T.Text
    correctAuthorNameOrder name = T.intercalate " " $ map T.strip splitReversed
      where
        splitReversed = reverse $ T.splitOn "," name

scrapeBuchhandel :: Isbn -> IO (Maybe CrawlResult)
scrapeBuchhandel isbn = do
  initRequest <- parseRequest lookupUrl
  let request = addRequestHeader "Accept" "*/*" $ addRequestHeader "User-Agent" "Mozilla/5.0 (Windows NT 6.3; Win64; x64; rv:68.0) Gecko/20100101 Firefox/68.0" initRequest
  response <- httpBS request

  guard $ getResponseStatusCode response == 200

  let body = BSL.fromStrict $ getResponseBody response

  writeFile "/tmp/test/output.json" (BSC.unpack $ BSL.toStrict body)

  case A.decode body of
    Nothing -> return Nothing
    Just val  -> do
      return $ A.parseMaybe startParser val

  where
    lookupUrl = "https://www.buchhandel.de/jsonapi/productDetails/" ++ show isbn

    startParser :: A.Value -> A.Parser CrawlResult
    startParser val = flip (A.withObject "data") val $ \obj -> do
      inner <- obj A..: "data"
      _resultFromValue isbn inner
