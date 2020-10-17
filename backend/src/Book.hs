{-# LANGUAGE DeriveGeneric #-}

module Book
  ( Book(..)
  , Isbn(..)
  , IsbnParseError()
  , isbnFromString
  , toIsbn13
  , jsonToBook
  , bookToJson
  , example
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as BSL
import           Data.Char
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           GHC.Generics

data Isbn
  = Isbn10 [Int]
  | Isbn13 [Int]
  deriving (Generic)

instance Show Isbn where
  show (Isbn10 numbers) = map isbnIntToDigit numbers
    where
      isbnIntToDigit 10 = 'X'
      isbnIntToDigit a  = intToDigit a
  show (Isbn13 numbers) = map intToDigit numbers

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

example :: Book
example = Book
 { _title = "This is a book"
 , _author = ["Author", "are"]
 , _isbn = Isbn10 [0,3,0,6,4,0,6,1,5,2]
 , _summary = "This is a book"
 , _language = "De"
 , _pages = 20
 }

data Book = Book
  { _title    :: String
  , _isbn     :: Isbn
  , _author   :: [String]
  , _summary  :: String
  , _pages    :: Int
  , _language :: String
  } deriving (Generic, Show)

instance A.ToJSON Isbn where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.ToJSON Book where
  toEncoding = A.genericToEncoding A.defaultOptions

instance A.FromJSON Isbn
instance A.FromJSON Book

bookToJson :: Book -> T.Text
bookToJson b = T.decodeUtf8 $ BSL.toStrict $ A.encode b

jsonToBook :: T.Text -> Either String Book
jsonToBook text = A.eitherDecode (BSL.fromStrict $ T.encodeUtf8 text)
