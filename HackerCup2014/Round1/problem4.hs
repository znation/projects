import Data.List
import Numeric
import System.IO

-- Problem 4 -- Preventing Altzheimers

main :: IO ()
main = do
    inFile <- openFile "problem4.txt" ReadMode
    contents <- hGetContents inFile
    hSetNewlineMode stdout noNewlineTranslation
    putStr $ generateOutput contents
    hClose inFile

generateOutput contents = ""
{-
    let firstLine = head $ lines contents
        t = read firstLine :: Int
    in  generateOutput' (tail $ lines contents) 1
-}

gcd' zs = gcd (head zs) (head (tail zs))

divisibleSet :: Int -> Int -> [[Int]]
divisibleSet n k =
    let divisible xs =
            let listGcd ys =
                    let ps = permutations ys
                        ft = map (take 2) ps
                        gcds = map gcd' ft
                    in  maximum gcds
            in  (listGcd xs) == k 
        incrementSet xs =
            let chars = map toBase20Char xs
                int = fst (head (readInt 20 validBase20Char fromBase20Char chars))
                inc = int + 1
                newChars = showIntAtBase 20 toBase20Char inc ""
            in  map fromBase20Char newChars
        divisibleSet' xs l =
            let rem = divisibleSet' (incrementSet xs) l
            in  if      l == (length xs)
                then    if      divisible xs
                        then    xs:rem
                        else    rem
                else    []
    in  divisibleSet' [1..n] (length [1..n])

toBase20Char :: Int -> Char
toBase20Char x =
    if      x >= 0 && x <= 9
    then    toEnum (x + 48)
    else    if      x >= 10 && x <= 19
            then    toEnum (x + 87)
            else    error ("invalid base20 number: " ++ (show x))

fromBase20Char :: Char -> Int
fromBase20Char c =
    let x = fromEnum c
    in  if      x >= 48 && x <= 57
        then    x - 48
        else    if      x >= 97 && x <= 106
                then    x - 87
                else    error ("invalid base20 char: " ++ (show c))

validBase20Char :: Char -> Bool
validBase20Char c =
    let x = fromEnum c
    in  (x >= 48 && x <= 57) || (x >= 97 && x <= 106)

