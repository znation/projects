module Problem017 where

answer :: Integer
answer = count (concat (map name [1..1000]))

count :: [a] -> Integer
count [] = 0
count (_:xs) = 1 + (count xs)

name :: Int -> String
name 1 = "one"
name 2 = "two"
name 3 = "three"
name 4 = "four"
name 5 = "five"
name 6 = "six"
name 7 = "seven"
name 8 = "eight"
name 9 = "nine"
name 10 = "ten"
name 11 = "eleven"
name 12 = "twelve"
name 13 = "thirteen"
name 14 = "fourteen"
name 15 = "fifteen"
name 16 = "sixteen"
name 17 = "seventeen"
name 18 = "eighteen"
name 19 = "nineteen"
name 20 = "twenty"
name 30 = "thirty"
name 40 = "forty"
name 50 = "fifty"
name 60 = "sixty"
name 70 = "seventy"
name 80 = "eighty"
name 90 = "ninety"
name 1000 = "onethousand"
name x =   if      x < 100
            then    let ones = x `mod` 10
                        tens = x - ones
                    in  (name tens) ++ (name ones)
            else    let hundreds = x `div` 100
                        rest = x `mod` 100
                    in  if      rest == 0
                        then    (name hundreds) ++ "hundred"
                        else    (name hundreds) ++ "hundredand" ++ (name rest)
                        
