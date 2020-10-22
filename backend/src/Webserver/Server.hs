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
  , ServerConfig(..)
  )
  where

import           Book
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BSC
import           Data.List
import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Proxy
import qualified Data.Text                   as T
import           Network.Wai
import           Network.Wai.Middleware.Cors
import           Scraping.BuchhandelScraper
import           Scraping.GoodreadsScraper
import           Servant
import           Storage

type UnsecureBookApi =
       -- GET /books
       "books" :> Get '[JSON] [Book]
       -- GET /book/:isbn
  :<|> "book" :> Capture "isbn" Isbn :> Get '[JSON] Book
       -- PUT /book/:isbn
  :<|> "book" :> Capture "isbn" Isbn :> Put '[JSON] Book
       -- GET /book/:isbn/cover
  :<|> "book" :> Capture "isbn" Isbn :> "cover" :> Get '[OctetStream] BS.ByteString
       -- PUT /book/:isbn/location [Body=location name]
  :<|> "book" :> Capture "isbn" Isbn :> "location" :> ReqBody '[PlainText] T.Text :> Put '[JSON] Book
       -- PUT /book/:isbn/location [Body=location name]
  :<|> "book" :> Capture "isbn" Isbn :> "location" :> Delete '[JSON] Book
       -- GET /search?title=...&summary=...&location=...
  :<|> "search"
    :> QueryParam "title" T.Text
    :> QueryParam "summary" T.Text
    :> QueryParam "location" T.Text
    :> Get '[JSON] [Book]
  :<|> "location" :> Get '[JSON] [T.Text]
  :<|> EmptyAPI

type BookApi = BasicAuth "librarium" User :> UnsecureBookApi

data ServerConfig = ServerConfig
 { serverDataDir :: FilePath
 , serverUsers   :: [(String, String)]
 , serverBooks   :: MVar (Map.Map Isbn Book)
}

type AppMonad = ReaderT ServerConfig Handler

orElse :: Maybe a -> a -> a
orElse Nothing a  = a
orElse (Just a) _ = a

_buildError :: ServerError -> String -> ServerError
_buildError baseError msg = baseError { errBody = encode $ object ["error" .= msg]}

_scrape :: Isbn -> IO (Maybe CrawlResult)
_scrape isbn = scrapeGoodreads isbn <|> scrapeBuchhandel isbn

server :: ServerT BookApi AppMonad
server _ = allBooks
  :<|> bookByIsbn :<|> addBookByIsbn :<|> coverByIsbn
  :<|> addOrOverwriteBookLocation :<|> deleteBookLocation
  :<|> searchBooks
  :<|> allLocations
  :<|> emptyServer
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

    coverByIsbn :: Isbn -> AppMonad BS.ByteString
    coverByIsbn isbn = do
      dir <- bookDir
      coverMaybe <- liftIO $ loadCover dir isbn

      case coverMaybe of
        Nothing   -> throwError $ _buildError err404 "Book not found"
        (Just bs) -> return bs

    addOrOverwriteBookLocation :: Isbn -> T.Text -> AppMonad Book
    addOrOverwriteBookLocation isbn location = setBookLocation isbn (Just location)

    deleteBookLocation :: Isbn -> AppMonad Book
    deleteBookLocation isbn = setBookLocation isbn Nothing

    setBookLocation :: Isbn -> Maybe T.Text -> AppMonad Book
    setBookLocation isbn location = do
      book <- bookByIsbn isbn
      let updatedBook = book { _location = location }
      addBook updatedBook Nothing

      return updatedBook

    allLocations :: AppMonad [T.Text]
    allLocations = mapMaybe _location <$> allBooks

    searchBooks :: Maybe T.Text -> Maybe T.Text -> Maybe T.Text -> AppMonad [Book]
    searchBooks titleFragment summaryFragment locationFragment = do
      books <- allBooks
      let predicate b = all (\ p -> p b) predicates
      return $ filter predicate books
      where
        predicates =
          [ booksByTitle titleFragment
          , booksBySummary summaryFragment
          , booksByLocation locationFragment
          ]

    booksByTitle :: Maybe T.Text -> Book -> Bool
    booksByTitle Nothing              _    = True
    booksByTitle (Just titleFragment) book = loweredFragment `T.isInfixOf` T.toLower (_title book)
      where
        loweredFragment = T.toLower titleFragment

    booksBySummary :: Maybe T.Text -> Book -> Bool
    booksBySummary Nothing                _    = True
    booksBySummary (Just summaryFragment) book = orElse checkSummary False
      where
        loweredFragment = T.toLower summaryFragment
        checkSummary = (loweredFragment `T.isInfixOf`) . T.toLower <$> _summary book

    booksByLocation :: Maybe T.Text -> Book -> Bool
    booksByLocation Nothing  _           = True
    booksByLocation (Just location) book = orElse checkLocation False
      where
        loweredFragment = T.toLower location
        checkLocation = (loweredFragment `T.isInfixOf`) . T.toLower <$> _location book

    addBookByIsbn :: Isbn -> AppMonad Book
    addBookByIsbn isbn = do
      crawlResult <- liftIO $ _scrape isbn

      case crawlResult of
        Nothing -> do
          dir <- bookDir
          liftIO $ addFailedIsbn dir isbn
          throwError $ _buildError err404 "Book not found :/"
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

apiContextProxy :: Proxy (ErrorFormatter ': BasicAuthCheck User ': '[])
apiContextProxy = Proxy

_runReader :: ServerConfig -> AppMonad a -> Handler a
_runReader s x = runReaderT x s

app :: ServerConfig -> Application
app config =
  cors (const $ Just policy) $
  serveWithContext bookApi
  (customFormatters :. basicAuthServerContext config)
  $ hoistServerWithContext bookApi apiContextProxy (_runReader config) server
  where
    policy = simpleCorsResourcePolicy
           { corsRequestHeaders = [ "Content-Type", "authorization" ]
           , corsMethods = [ "DELETE", "GET", "POST", "PUT" ]
           }

newtype User = User { userName :: String } deriving (Show)

authCheck :: ServerConfig -> BasicAuthCheck User
authCheck config =
  let check (BasicAuthData username password) =
        if orElse (passwordMatches (username, password)) False
        then return $ Authorized $ User $ BSC.unpack username
        else return Unauthorized
  in BasicAuthCheck check
  where
    users :: [(String, String)]
    users = serverUsers config

    getPassword :: BSC.ByteString -> Maybe BSC.ByteString
    getPassword name = BSC.pack . snd <$> find ((==name) . BSC.pack . fst) users

    passwordMatches :: (BSC.ByteString, BSC.ByteString) -> Maybe Bool
    passwordMatches (user, password) = (== password) <$> getPassword user

basicAuthServerContext :: ServerConfig -> Context (BasicAuthCheck User ': '[])
basicAuthServerContext config = authCheck config :. EmptyContext

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
