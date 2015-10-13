module DoubledWords
       (
         OurWord(..)
       , WordInfo(..)
       , doubledWords
       ) where

-- | Our definition of a word, once parsing is complete.
--
-- (newtype is Haskell's zero-runtime-cost wrapper to avoid
-- <http://c2.com/cgi/wiki?PrimitiveObsession primitive obsession>.)
newtype OurWord =
  OurWord { _text :: String }
  deriving (Eq, Show)

-- | For each doubled word, keep track of information to report.
data WordInfo =
  WordInfo { _line :: Int     -- ^ latest line number of a run of duplicates
           , _word :: OurWord -- ^ the word doubled
           }
  deriving (Eq, Show)

-- | Return information about each run of doubled words.
doubledWords :: String
             -> [WordInfo]
doubledWords = undefined
