-----------------------------------------------------------------------------
-- |
-- Module      :  GeneratorUtils
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Util methods for any generators
-----------------------------------------------------------------------------

module GeneratorUtils
  ( uniqueFilter,
    randomNumbers,
    randomNumberByRange,
    Amount,
    Offset,
    Range
  )
where
  
import System.Random
  
type Offset = Int

type Amount = Int

type DigitsAmount = Int

type Range = (Int, Int)

uniqueFilter :: (Eq a) => [a] -> [a]
uniqueFilter [] = []
uniqueFilter (x : xs) = x : uniqueFilter (filter (/= x) xs)

randomNumbers :: Offset -> Amount -> [Int]
randomNumbers offset amount =
  let generated = randoms $ mkStdGen offset :: [Int]
   in map abs $ take amount generated

randomNumberByRange :: Range -> Offset -> Int
randomNumberByRange range offset =
  let digits = length $ show $ fst range
      numByRange = randomNumber offset digits
   in rangeHelper range offset numByRange

rangeHelper :: Range -> Offset -> Int -> Int
rangeHelper (l, h) offset number
 | number >= l && number <= h = number
 | otherwise = randomNumberByRange (l, h) (offset + 1)

randomNumber :: Offset -> DigitsAmount -> Int
randomNumber offset digitsAmount = 
  let numAsStr = take digitsAmount . show $ head (randomNumbers offset digitsAmount)
   in read numAsStr :: Int