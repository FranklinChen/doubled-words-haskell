module DoubledWords
       (
         OurWord(..)
       , WordInfo(..)
       , doubledWords
       , keepOnlyLatestDuplicates
       ) where

import Control.Arrow ((>>>))
import qualified Data.List as List

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
  stringToWordInfos
  >>> keepOnlyLatestDuplicates

-- | Analyze a string all the way to line-tagged words.
stringToWordInfos :: String -> [WordInfo]
stringToWordInfos =
  lines
  >>> zipWith lineToWordInfos [1..]
  >>> concat

-- | Distribute a line number into parsing a line into words.
lineToWordInfos :: Int        -- ^ 1-based line number
                -> String     -- ^ a line of text
                -> [WordInfo] -- ^ info for each word in the line
lineToWordInfos lineNumber =
  words
  >>> map (OurWord >>> WordInfo lineNumber)

-- | Rid info for words without adjacent duplicates, and
-- keep only the latest of each run of duplicates.
keepOnlyLatestDuplicates :: [WordInfo] -> [WordInfo]
keepOnlyLatestDuplicates =
  List.groupBy isSameWord
  >>> filter isDuplicatedGroup
  >>> map last

-- | Whether two words are the same, for the purpose of detecting
-- duplicates. Here we just compare the chars.
isSameWord :: WordInfo -> WordInfo -> Bool
isSameWord info1 info2 =
  _word info1 == _word info2

-- | A group with at least 2 elements is duplicated.
isDuplicatedGroup :: [WordInfo] -> Bool
isDuplicatedGroup (_:_:_) = True
isDuplicatedGroup _ = False
