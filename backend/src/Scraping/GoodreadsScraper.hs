{-# LANGUAGE OverloadedStrings #-}

module Scraping.GoodreadsScraper
  ( CrawlResult(..)
  , scrapeGoodreads
  )
  where

import           Book
import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Text           as T
import           Text.HTML.Scalpel

data CrawlResult = CrawlResult
  { _book       :: Book
  , _coverImage :: Maybe String
  }
  deriving (Show)

_goodreadsScraper :: Isbn -> Scraper T.Text CrawlResult
_goodreadsScraper isbn = CrawlResult <$> book <*> optional coverImageUrl
  where
    book :: Scraper T.Text Book
    book = Book
      <$> title
      <*> pure isbn
      <*> authors
      <*> optional description
      <*> optional pages
      <*> optional language
      <*> pure Nothing
      <*> pure "Goodreads"

    title :: Scraper T.Text T.Text
    title = chroot (AnyTag @: ["id" @= "bookTitle"]) $ T.strip <$> text anySelector

    pages :: Scraper T.Text Int
    pages = chroot (AnyTag @: ["itemprop" @= "numberOfPages"]) $ do
      string <- filter isDigit . T.unpack . T.strip <$> text anySelector
      return $ read string

    language :: Scraper T.Text T.Text
    language = chroot (AnyTag @: ["itemprop" @= "inLanguage"]) $
      T.strip <$> text anySelector

    description :: Scraper T.Text T.Text
    description = chroot (AnyTag @: ["id" @= "description"] // "span") $ do
      pos <- position
      guard $ pos == 1
      T.strip <$> text anySelector

    authors :: Scraper T.Text [T.Text]
    authors = chroots (AnyTag @: [hasClass "authorName", notP (hasClass "role")]) $
      T.strip <$> text anySelector

    coverImageUrl :: Scraper T.Text String
    coverImageUrl = chroot (AnyTag @: ["id" @= "coverImage"]) $
      T.unpack . T.strip <$> attr "src" anySelector

scrapeGoodreads :: Isbn -> IO (Maybe CrawlResult)
scrapeGoodreads isbn = scrapeURLWithConfig config url scraper
  where
    config = Config utf8Decoder Nothing
    url = "https://www.goodreads.com/search?q=" ++ show isbn
    scraper = _goodreadsScraper isbn
