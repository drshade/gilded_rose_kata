module Main where

import GildedRose
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "updateQuality" $ do
        it "fixme (first existing test provided)" $
            updateQuality [Item "foo" 0 0] `shouldBe` [Item "foo" (-1) 0]
        it "should degrade twice as fast after sell by date" $
            updateQuality [Item "foo" (-1) 5] `shouldBe` [Item "foo" (-2) 3]
        it "quality of an item should never be negative" $ do
            updateQuality [Item "foo" 3 (-1)] `shouldBe` [Item "foo" 2 0]
        it "aged brie increases in quality the older it gets" $ do
            updateQuality [Item "Aged Brie" 3 4] `shouldBe` [Item "Aged Brie" 2 5]
        it "the quality of an item is never more than 50" $ do
            updateQuality [Item "Aged Brie" 3 50] `shouldBe` [Item "Aged Brie" 2 50]
            updateQuality [Item "foo" 3 555] `shouldBe` [Item "foo" 2 50]
        it "sulfuras never needs to be sold, nor decreases in quality" $ do
            updateQuality [Item "Sulfuras" 10 20] `shouldBe` [Item "Sulfuras" 10 20]
        it "backstage passes increase in value as sellin value approaches (above 10 days)" $ do
            updateQuality [Item "Backstage passes" 20 10] `shouldBe` [Item "Backstage passes" 19 11]
        it "backstage passes increase in value as sellin value approaches (10 days or less)" $ do
            updateQuality [Item "Backstage passes" 10 10] `shouldBe` [Item "Backstage passes" 9 12]
        it "backstage passes increase in value as sellin value approaches (5 days or less)" $ do
            updateQuality [Item "Backstage passes" 5 10] `shouldBe` [Item "Backstage passes" 4 13]
        it "backstage passes quality drops to zero after concert" $ do
            updateQuality [Item "Backstage passes" 0 10] `shouldBe` [Item "Backstage passes" (-1) 0]
        it "conjured items degrade in quality twice as fast" $ do
            updateQuality [Item "Conjured" 10 20] `shouldBe` [Item "Conjured" 9 18]
            updateQuality [Item "Conjured" (-10) 20] `shouldBe` [Item "Conjured" (-11) 16]
