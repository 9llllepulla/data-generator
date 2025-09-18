module Main (main) where

-- import CommandLine
-- import GeneratorUtils
-- import PersonalityGenerators
-- import System.Environment
import PhoneOutput

{-main :: IO ()
main = do
  args <- getArgs
  putStrLn $ commandArgs args-}

main :: IO ()
main = do
    --    printRandomPhonesOnConsole 7 3
    printRandomPhonesToFile 7 10 "10_mobile.txt"
    putStrLn "Job well done!"
