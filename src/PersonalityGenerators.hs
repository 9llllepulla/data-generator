-----------------------------------------------------------------------------
-- |
-- Module      :  PersonalityGenerators
-- Copyright   :  (c) Sergey Lyashko 2023
-- License     :  see LICENSE
--
-- Generator for personality full names
-----------------------------------------------------------------------------

module PersonalityGenerators (fullNamesGen, randomBirthDay, generationFullName) where

import GeneratorUtils  
import Data.Char (toUpper)
import PhoneGenerators
import System.Random

type Name = String

type LastName = String

type BirthDay = String

data FullName = FullName Name LastName BirthDay deriving (Show)

instance Generated FullName where
  toString (FullName name lastName birthDay) = capitalize name ++ "," ++ capitalize lastName ++ "," ++ birthDay

capitalize :: String -> String
capitalize [] = []
capitalize (x : xs) = toUpper x : xs

-- генерация списка уникальных случайных имени-фамилии
fullNamesGen :: Range -> Amount -> Offset -> [String]
fullNamesGen range amount offset = 
  let numbers = take amount [1 ..]
   in take amount $ uniqueFilter . map (\x -> toString $ fullNameGen range (x + offset)) $ numbers

fullNameGen :: Range -> Offset -> FullName
fullNameGen range offset =
  let nameLen = randomNumberByRange (3, 7) offset
      lastNameLen = randomNumberByRange (3, 9) offset
   in generationFullName range (nameLen, lastNameLen) offset

generationFullName :: Range -> (Int, Int) -> Offset -> FullName
generationFullName range (nameLen, lastNameLen) offset =
  let name = randomAnyName nameLen offset
      lastName = randomAnyName lastNameLen (offset + 1)
      birthDay = randomBirthDay range offset
   in FullName name lastName birthDay

randomAnyName :: Amount -> Offset -> String
randomAnyName charsAmount offset = take charsAmount (randomRs ('a', 'z') $ mkStdGen $ charsAmount * offset)

randomBirthDay :: Range -> Offset -> String
randomBirthDay range offset =
  let day = take 2 $ show $ randomNumberByRange (1, 30) offset
      month = take 2 $ show $ randomNumberByRange (1, 12) (offset + 1)
      year = take 4 $ show $ randomNumberByRange range (offset + 2)
   in day ++ "." ++ month ++ "." ++ year