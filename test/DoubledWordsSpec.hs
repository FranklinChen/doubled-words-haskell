module DoubledWordsSpec (main, spec) where

import Test.Hspec

import DoubledWords (OurWord(..), WordInfo(..), doubledWords)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "DoubledWords" $ do
    it "reports nothing when there are no doubled words" $ do
      doubledWords "hello world" `shouldBe` []
    it "gives the furthest down line numbers for a doubled word" $ do
      doubledWords "hello\nhello\nhello" `shouldBe`
        [ WordInfo 3 (OurWord "hello") ]
    it "handles a realistic stream of different doubled words" $ do
      doubledWords "hello\nhello\n  hello\n world\nthere here here" `shouldBe`
        [ WordInfo 3 (OurWord "hello")
        , WordInfo 5 (OurWord "here")
        ]
