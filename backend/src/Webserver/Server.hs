{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Webserver.Server
  ( server
  , bookApi
  , app
  , main
  )
  where

import           Book
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Proxy
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Scraping.GoodreadsScraper
import           Servant
import           Storage


type BookApi =
       -- GET /books
       "books" :> Get '[JSON] [Book]
       -- GET /book/:isbn
  :<|> "book" :> Capture "isbn" Isbn :> Get '[JSON] Book
  :<|> "book" :> Capture "isbn" Isbn :> Put '[JSON] Book
  :<|> EmptyAPI

server :: Server BookApi
server = books :<|> bookByIsbn :<|> addBookByIsbn :<|> emptyServer
  where
    books :: Handler [Book]
    books = liftIO $ listAllBooks "/tmp/test"

    bookByIsbn :: Isbn -> Handler Book
    bookByIsbn isbn = do
      maybeBook <- liftIO $ loadBook "/tmp/test" isbn

      case maybeBook of
        (Left err) -> throwError $ err404 { errBody = BS.pack err }
        (Right book) -> return book

    addBookByIsbn :: Isbn -> Handler Book
    addBookByIsbn isbn = do
      crawlResult <- liftIO $ scrapeGoodreads isbn

      case crawlResult of
        Nothing -> throwError $ err404 { errBody = "Book not found :/" }
        (Just result) -> do
          case _coverImage result of
            (Just url) -> do liftIO $ storeCover "/tmp/test" isbn url
            Nothing    -> pure ()
          liftIO $ storeBook "/tmp/test" (_book result)
          return (_book result)


bookApi :: Proxy BookApi
bookApi = Proxy

app :: Application
app = serveWithContext bookApi (customFormatters :. EmptyContext) server

main :: IO ()
main = run 8081 app

customFormatter :: ErrorFormatter
customFormatter tr _ err =
  let
    value = object ["combinator" .= show tr, "error" .= err]
  in
    err400
      { errBody = encode value
      , errHeaders = [("Content-Type", "application/json")]
      }

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { urlParseErrorFormatter = customFormatter
  , bodyParserErrorFormatter = customFormatter
  }
