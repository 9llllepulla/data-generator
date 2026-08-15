{-
-----------------------------------------------------------------------------
    Module      :  PersonalityGenerators
    Copyright   :  (c) Sergey Lyashko 2023-2026
    License     :  see LICENSE
-----------------------------------------------------------------------------
-}

-- | Generator for phone numbers by prefix
{-# LANGUAGE InstanceSigs #-}
module PhoneGenerators
  ( orderedPhonesGen
  , randomPhoneGen
  , PhonePrefix
  , Generated(..)
  ) where

import           GeneratorUtils

class Generated a where
  toString :: a -> String

instance Generated PhoneNumber where
  toString :: PhoneNumber -> String
  toString (PhoneNumber prefix number) = show prefix ++ show number

data PhoneNumber =
  PhoneNumber PhonePrefix Int
  deriving (Show)

type PhonePrefix = Int

-- генератор заданного количества уникальных случайных номеров телефонов с префиксом по коэффициенту смещения
randomPhoneGen :: PhonePrefix -> Amount -> Offset -> [String]
randomPhoneGen prefix amount offset =
  let nums = randomNumbers offset amount
   in take amount $ uniqueFilter $ map (take 11 . phoneWith prefix) nums

-- генератор заданного количества номеров телефонов по префиксу
orderedPhonesGen :: PhonePrefix -> Amount -> [String]
orderedPhonesGen prefix amount =
  map (phoneWith prefix) $ take amount [1000000000 ..]

phoneWith :: PhonePrefix -> Int -> String
phoneWith prefix = toString . PhoneNumber prefix
