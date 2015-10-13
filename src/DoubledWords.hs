module DoubledWords
       (
         OurWord(..)
       , WordInfo(..)
       , doubledWords
       ) where

import Control.Arrow ((>>>))

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
doubledWords =
  lines
  >>> zipWith lineToWordInfos [1..]
  >>> concat

-- | Distribute a line number into parsing a line into words.
lineToWordInfos :: Int        -- ^ 1-based line number
                -> String     -- ^ a line of text
                -> [WordInfo] -- ^ info for each word in the line
lineToWordInfos = undefined
