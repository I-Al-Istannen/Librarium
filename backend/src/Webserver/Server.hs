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
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.Map                   as Map
import           Data.Proxy
import qualified Data.Text                  as T
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
  :<|> "search" :> QueryParam "title" T.Text :> Get '[JSON] [Book]
  :<|> EmptyAPI

data ServerConfig = ServerConfig {
  serverDataDir :: FilePath,
  serverBooks   :: MVar (Map.Map Isbn Book)
}

type AppMonad = ReaderT ServerConfig Handler

_buildError :: ServerError -> String -> ServerError
_buildError baseError msg = baseError { errBody = encode $ object ["error" .= msg]}

server :: ServerT BookApi AppMonad
server = allBooks :<|> bookByIsbn :<|> addBookByIsbn :<|> booksByTitle :<|> emptyServer
  where
    bookDir :: AppMonad FilePath
    bookDir = serverDataDir <$> ask

    bookMap :: AppMonad (Map.Map Isbn Book)
    bookMap = do
       mVar <- serverBooks <$> ask
       liftIO $ readMVar mVar

    allBooks :: AppMonad [Book]
    allBooks = Map.elems <$> bookMap

    bookByIsbn :: Isbn -> AppMonad Book
    bookByIsbn isbn = do
      maybeBook <- (Map.!? isbn) <$> bookMap

      case maybeBook of
        Nothing     -> throwError $ _buildError err404 "Book not found"
        (Just book) -> return book

    booksByTitle :: Maybe T.Text -> AppMonad [Book]
    booksByTitle Nothing              = allBooks
    booksByTitle (Just titleFragment) = filter predicate <$> allBooks
      where
        predicate :: Book -> Bool
        predicate book = titleFragment `T.isInfixOf` T.pack (_title book)

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
      liftIO $ modifyMVar_ books (pure . Map.insert isbn book)


bookApi :: Proxy BookApi
bookApi = Proxy

_runReader :: ServerConfig -> AppMonad a -> Handler a
_runReader s x = runReaderT x s

app :: ServerConfig -> Application
app config = serveWithContext bookApi (customFormatters :. EmptyContext) $ hoistServer bookApi (_runReader config) server


main :: IO ()
main = do
  initialBooks <- newEmptyMVar
  let config = ServerConfig { serverDataDir = "/tmp/test", serverBooks = initialBooks }

  allBooks <- listAllBooks $ serverDataDir config
  let bookMap = map (\b -> (_isbn b, b)) allBooks
  putMVar initialBooks $ Map.fromList bookMap

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
