module Main where

import Control.Arrow ((>>>))
import Data.Function ((&))
import Text.Printf (printf)

import DoubledWords (OurWord(..), WordInfo(..), doubledWords)

-- | Read from standard input, analyze for doubled words, print a report.
--
-- Note an interesting feature: because of laziness, this program will
-- immediately report a run of duplicates as soon as it sees the end of one.
main :: IO ()
main = getContents
       >>= (doubledWords >>> printReport)

printReport :: [WordInfo] -> IO ()
printReport = mapM_ printWordInfo

-- | Print report to standard output.
printWordInfo :: WordInfo -> IO ()
printWordInfo info =
  printf "%d %s\n" (info & _line) (info & _word & _text)
