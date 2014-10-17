module Problem042 where

import Data.Char
import Utility

fileName :: String
fileName = "Problem042_words.txt"

answer :: [String] -> Int
answer input = length (filter triangleWord input)

triangleWord :: String -> Bool
triangleWord str =  let ds = map charToPosition str
                    in  triangleNumber (sum ds)

charToPosition :: Char -> Int
charToPosition c = (ord c) - 64
