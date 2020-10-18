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
  , ServerConfig
  )
  where

import           Book
import           Control.Monad.IO.Class
import           Data.Aeson
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

data ServerConfig = ServerConfig {
  serverDataDir :: FilePath
} deriving (Show)

_buildError :: ServerError -> String -> ServerError
_buildError baseError msg = baseError { errBody = encode $ object ["error" .= msg]}

server :: ServerConfig -> Server BookApi
server config = books :<|> bookByIsbn :<|> addBookByIsbn :<|> emptyServer
  where
    bookDir = serverDataDir config

    books :: Handler [Book]
    books = liftIO $ listAllBooks bookDir

    bookByIsbn :: Isbn -> Handler Book
    bookByIsbn isbn = do
      maybeBook <- liftIO $ loadBook bookDir isbn

      case maybeBook of
        (Left err)   -> throwError $ _buildError err404 err
        (Right book) -> return book

    addBookByIsbn :: Isbn -> Handler Book
    addBookByIsbn isbn = do
      crawlResult <- liftIO $ scrapeGoodreads isbn

      case crawlResult of
        Nothing -> throwError $ _buildError err404 "Book not found :/"
        (Just result) -> do
          case _coverImage result of
            (Just url) -> do liftIO $ storeCover bookDir isbn url
            Nothing    -> pure ()
          liftIO $ storeBook bookDir (_book result)
          return (_book result)


bookApi :: Proxy BookApi
bookApi = Proxy

app :: Application
app = serveWithContext bookApi (customFormatters :. EmptyContext) $ server $ ServerConfig
  { serverDataDir = "/tmp/test"
  }

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
