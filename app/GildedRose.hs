module GildedRose where

import Prelude hiding (max, min)

type GildedRose = [Item]

data Item = Item String Int Int
    deriving (Eq)

instance Show Item where
    show (Item name sellIn quality) =
        name ++ ", " ++ show sellIn ++ ", " ++ show quality

-- Ensure a value stays between min and max values
clamp :: (Int, Int) -> Int -> Int
clamp (min, max) val
    | val <= min = min
    | val >= max = max
    | otherwise = val

-- Decrement a value while clamping between 0 & 50
decr :: Int -> Int
decr = clamp (0, 50) . pred

-- Increment a value while clamping between 0 & 50
incr :: Int -> Int
incr = clamp (0, 50) . succ

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateItem
  where
    updateItem (Item name@"Aged Brie" sellIn quality) = Item name (sellIn - 1) (incr quality)
    updateItem (Item name@"Sulfuras" sellIn quality) = Item name sellIn quality
    updateItem (Item name@"Backstage passes" sellIn quality)
        | sellIn <= 0 = Item name (sellIn - 1) 0
        | sellIn <= 5 = Item name (sellIn - 1) (incr $ incr $ incr $ quality)
        | sellIn <= 10 = Item name (sellIn - 1) (incr $ incr $ quality)
        | otherwise = Item name (sellIn - 1) (incr quality)
    updateItem (Item name@"Conjured" sellIn quality)
        | sellIn <= 0 = Item name (sellIn - 1) (clamp (0, 50) $ quality - 4)
        | otherwise = Item name (sellIn - 1) (clamp (0, 50) $ quality - 2)
    updateItem (Item name sellIn quality)
        | sellIn <= 0 = Item name (sellIn - 1) (clamp (0, 50) $ quality - 2)
        | otherwise = Item name (sellIn - 1) (clamp (0, 50) $ quality - 1)
