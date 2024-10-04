module Main where

import GildedRose
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "updateQuality" $ do
        it "fixme" $
            let inventory = [Item "foo" 0 0]
                actual = updateQuality inventory
                expected = []
             in actual `shouldBe` expected
