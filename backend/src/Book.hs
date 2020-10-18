{-# LANGUAGE DeriveGeneric #-}

module Book
  ( Book(..)
  , Isbn(..)
  , IsbnParseError()
  , isbnFromString
  , toIsbn13
  , jsonToBook
  , bookToJson
  , mapLeft
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           GHC.Generics
import           Servant

data Isbn
  = Isbn10 [Int]
  | Isbn13 [Int]
  deriving (Generic)

instance A.ToJSON Isbn where
  toJSON isbn = A.toJSON $ show isbn
instance A.FromJSON Isbn where
  parseJSON = A.withText "isbn string" $ \isbnString -> do
    case isbnFromString (T.unpack isbnString) of
      (Right isbn)  -> pure isbn
      (Left errors) -> fail $ intercalate ", " $ map show errors

instance Eq Isbn where
  a == b = show a == show b

instance Ord Isbn where
  a <= b = show a <= show b

instance Show Isbn where
  show (Isbn10 numbers) = map isbnIntToDigit numbers
    where
      isbnIntToDigit 10 = 'X'
      isbnIntToDigit a  = intToDigit a
  show (Isbn13 numbers) = map intToDigit numbers

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left x)  = Left $ f x
mapLeft _ (Right x) = Right x

instance FromHttpApiData Isbn where
  -- parseUrlPiece :: T.Text -> Either T.Text a
  parseUrlPiece text = mapLeft (T.pack . intercalate ", " . map show) $ isbnFromString $ T.unpack text

data IsbnParseError
   = InvalidLength
   | InvalidChecksum
   deriving (Show)

_myGuard :: a -> Bool -> Except a ()
_myGuard _ True  = pure ()
_myGuard a False = throwE a

_isbn10Valid :: [Int] -> Bool
_isbn10Valid = (==0) . (`mod` 11) . sum . zipWith (*) [10, 9 .. 0]

_isbn10FromString :: String -> Except [IsbnParseError] Isbn
_isbn10FromString isbn = do
  let cleanIsbn = mapMaybe isbnDigitToInt isbn

  _myGuard [InvalidLength] $ length cleanIsbn == 10
  _myGuard [InvalidChecksum] $ _isbn10Valid cleanIsbn

  pure $ Isbn10 cleanIsbn
  where
    isbnDigitToInt 'X' = Just 10
    isbnDigitToInt n   = digitToInt n <$ guard (isDigit n)

_isbn13Valid :: [Int] -> Bool
_isbn13Valid = (==0) . (`mod` 10) . sum . zipWith (*) (cycle [1,3])

_isbn13FromString :: String -> Except [IsbnParseError] Isbn
_isbn13FromString isbn = do
  let cleanIsbn = map digitToInt $ filter isDigit isbn

  _myGuard [InvalidLength] $ length cleanIsbn == 13
  _myGuard [InvalidChecksum] $ _isbn13Valid cleanIsbn

  pure $ Isbn13 cleanIsbn

isbnFromString :: String -> Either [IsbnParseError] Isbn
isbnFromString n = runExcept (_isbn13FromString n <|> _isbn10FromString n)

_isbn13CheckDigit :: [Int] -> Int
_isbn13CheckDigit = (`mod` 10) . (\a -> -a) . sum . zipWith (*) (cycle [1, 3])

toIsbn13 :: Isbn -> Isbn
toIsbn13 (Isbn10 numbers) = let
  isbn13Numbers = [9, 7, 8] ++ init numbers
  finalDigit = _isbn13CheckDigit isbn13Numbers
  in Isbn13 $ isbn13Numbers ++ [finalDigit]
toIsbn13 isbn             = isbn

data Book = Book
  { _title     :: String
  , _isbn      :: Isbn
  , _author    :: [String]
  , _summary   :: Maybe String
  , _pages     :: Maybe Int
  , _language  :: Maybe String
  , _crawledBy :: String
  } deriving (Generic, Show)

instance A.ToJSON Book where
  toEncoding = A.genericToEncoding A.defaultOptions
instance A.FromJSON Book

bookToJson :: Book -> T.Text
bookToJson b = T.decodeUtf8 $ BSL.toStrict $ A.encode b

jsonToBook :: T.Text -> Either String Book
jsonToBook text = A.eitherDecode (BSL.fromStrict $ T.encodeUtf8 text)
