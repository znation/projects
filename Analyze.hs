import qualified IO
import qualified Quote
import qualified Random as Rand
 
-- MAIN
main :: IO ()
main = do
    inh <- IO.openFile "quotes.tsv" IO.ReadMode
    mylines <- readloop inh []
    r1 <- Rand.getStdGen
    let randoms = Rand.randoms r1 :: [Int]
    let quotes = Quote.makeQuotes (map words mylines)
    printlines quotes
    IO.hClose inh
 
readloop :: IO.Handle -> [String] -> IO [String]
readloop inh array = 
    do
        ineof <- IO.hIsEOF inh
        if ineof
            then return array
            else
				do
					inpStr <- IO.hGetLine inh
					readloop inh (inpStr:array)
 
printlines :: Show t => [t] -> IO ()
printlines (x:xs) =
    do
        putStrLn (show x)
        printlines xs
printlines _ = return ()
