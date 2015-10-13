module Main where

import Control.Arrow ((>>>))

import DoubledWords (WordInfo(..), doubledWords)

-- | Read from standard input, analyze for doubled words, print a report.
main :: IO ()
main = getContents
       >>= (doubledWords >>> printReport)

printReport :: [WordInfo] -> IO ()
printReport = undefined
