module Book
  ( Book(..)
  , Isbn()
  , IsbnParseError()
  , isbnFromString
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Char
import           Data.Maybe

data Isbn
  = Isbn10 [Int]
  | Isbn13 [Int]

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

data Book = Book
  { _title    :: String
  , _isbn     :: Isbn
  , _author   :: [String]
  , _summary  :: String
  , _pages    :: Int
  , _language :: String
  }
