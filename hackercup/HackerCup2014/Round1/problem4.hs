import Data.List
import Debug.Trace
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

generateOutput contents = generateOutput' (tail $ lines contents) 1

generateOutput' contents 3 = ""
generateOutput' contents n =
    "Case #" ++ (show n) ++ ": " ++ (generateOutput'' (take 2 contents)) ++ "\n" ++ (generateOutput' (drop 2 contents) (n+1))

generateOutput'' (firstLine:secondLine:_) =
    let n :: Int
        k :: Int
        n:k:_ = map (read :: String -> Int) (words firstLine)
        xs :: [Int]
        xs = sort $ map (read :: String -> Int) (words secondLine)
{-
        tooLow :: [Int] -> Bool
        tooLow ys =
            let tooLow' [] [] = False
                tooLow' (x:xs') (y:ys') =
                    if      y < x
                    then    True
                    else    tooLow' xs' ys'
            in  tooLow' xs ys
        sorted = sort (divisibleSet n k)
        matches :: [[Int]]
        matches = dropWhile tooLow sorted
        lowestMatch :: [Int]
        lowestMatch = (head matches)
-}
        lowestMatch :: [Int]
        lowestMatch =
            let lowestMatch' existing [] = []
                lowestMatch' existing remaining =
                    let ds = map divisors existing
                        ds' = map (fromList :: [a] -> Set a) ds
                        is = foldl1 intersection ds'
            in  lowestMatch' [head xs] [tail xs]
                        
        diff = (sum lowestMatch) - (sum xs)
    in  show diff

listGcd :: [Int] -> Int
listGcd (x1:x2:[]) = gcd x1 x2
listGcd (x1:x2:xs) = listGcd ((gcd x1 x2):xs)

properDivisors :: Integral a => a -> [a]
properDivisors =    let properDivisors' :: Integral a => a -> a -> [a]
                        properDivisors' i x =   let rest = properDivisors' (i+1) x
                                                in  if      i > x `div` 2
                                                    then    []
                                                    else    if      x `mod` i == 0
                                                            then    i:rest
                                                            else    rest
                    in  properDivisors' 1
                    
divisors :: Integral a => a -> [a]
divisors x = (properDivisors x) ++ [x]
                         
factors :: Integral a => a -> [a]
factors n = factors' 2 [] n

factors' :: Integral a => a -> [a] -> a -> [a]
factors' _ _ 0 = [0]
factors' _ _ 1 = [1]
factors' d acc x =  if      d > (isqrt x)
                    then    x:acc
                    else    let (result,remainder) = x `divMod` d
                            in  if      remainder == 0 -- factor
                                then    let a = factors result
                                            b = factors d
                                        in  a ++ b
                                else    factors' (d+1) acc x

{-
divisibleSet :: Int -> Int -> [[Int]]
divisibleSet n k =
        incrementSet xs =
            let chars = map toBase51Char xs
                int = fst (head (readInt 51 validBase51Char fromBase51Char chars))
                inc = int + 1
                newChars = showIntAtBase 51 toBase51Char inc ""
            in  map fromBase51Char newChars
        divisibleSet' xs l =
            let rem = divisibleSet' (incrementSet xs) l
            in  if      l == (length xs)
                then    if      divisible xs
                        then    xs:rem
                        else    rem
                else    []
    in  divisibleSet' [1..n] (length [1..n])
-----
    let divisible xs = (listGcd xs) == k 
        divisibleSet' (x:xs) =
            if      x > 50
            then    []
            else    let l = last xs
                        x' = l + k
                        next = xs ++ [x']
                    in  if      divisible next
                        then    next:(divisibleSet' next)
                        else    divisibleSet' next
    in  divisibleSet' [1..n]
-}

toBase51Char :: Int -> Char
toBase51Char x =
    if      x >= 0 && x < 51
    then    toEnum x
    else    error ("invalid base51 number: " ++ (show x))

fromBase51Char :: Char -> Int
fromBase51Char c =
    let x = fromEnum c
    in  if      x >= 0 && x < 51
        then    x
        else    error ("invalid base51 char: " ++ (show c))

validBase51Char :: Char -> Bool
validBase51Char c =
    let x = fromEnum c
    in  x >= 0 && x < 51

