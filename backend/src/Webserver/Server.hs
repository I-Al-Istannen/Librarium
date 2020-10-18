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
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Control.Monad.STM           (atomically)
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.List
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
       -- PUT /book/:isbn
  :<|> "book" :> Capture "isbn" Isbn :> Put '[JSON] Book
  :<|> EmptyAPI

data ServerConfig = ServerConfig {
  serverDataDir :: FilePath,
  serverBooks   :: TVar [Book]
}

type AppMonad = ReaderT ServerConfig Handler

_buildError :: ServerError -> String -> ServerError
_buildError baseError msg = baseError { errBody = encode $ object ["error" .= msg]}

server :: ServerT BookApi AppMonad
server = books :<|> bookByIsbn :<|> addBookByIsbn :<|> emptyServer
  where
    bookDir :: AppMonad FilePath
    bookDir = serverDataDir <$> ask

    books :: AppMonad [Book]
    books = do
      ServerConfig{serverBooks = allBooks} <- ask
      liftIO $ readTVarIO allBooks

    bookByIsbn :: Isbn -> AppMonad Book
    bookByIsbn isbn = do
      maybeBook <- find ((== isbn) . _isbn) <$> books

      case maybeBook of
        Nothing     -> throwError $ _buildError err404 "Book not found"
        (Just book) -> return book

    addBookByIsbn :: Isbn -> AppMonad Book
    addBookByIsbn isbn = do
      crawlResult <- liftIO $ scrapeGoodreads isbn

      case crawlResult of
        Nothing -> throwError $ _buildError err404 "Book not found :/"
        (Just result) -> do
          addBook (_book result) (_coverImage result)
          return (_book result)

    addBook :: Book -> Maybe String -> AppMonad ()
    addBook book coverUrl = do
      dir <- bookDir
      let isbn = _isbn book

      case coverUrl of
        (Just url) -> do liftIO $ storeCover dir isbn url
        Nothing    -> pure ()

      liftIO $ storeBook dir book

      ServerConfig{serverBooks = books} <- ask
      liftIO $ atomically $ readTVar books >>= writeTVar books . insertReplace book
      where
        insertReplace :: Book -> [Book] -> [Book]
        insertReplace book books = book : filter ( (_isbn book /=) . _isbn) books



bookApi :: Proxy BookApi
bookApi = Proxy

_runReader :: ServerConfig -> AppMonad a -> Handler a
_runReader s x = runReaderT x s

app :: ServerConfig -> Application
app config = serveWithContext bookApi (customFormatters :. EmptyContext) $ hoistServer bookApi (_runReader config) server


main :: IO ()
main = do
  initialBooks <- newTVarIO []
  let config = ServerConfig { serverDataDir = "/tmp/test", serverBooks = initialBooks }

  allBooks <- listAllBooks $ serverDataDir config
  atomically $ writeTVar initialBooks allBooks

  run 8081 (app config)

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
