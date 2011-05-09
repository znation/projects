import qualified Generation
import qualified IO
import qualified Quote
import qualified Seed

-- MAIN
main :: IO ()
main = do
    inh <- IO.openFile "quotes.tsv" IO.ReadMode
    mylines <- readloop inh []
    seeds <- Seed.randoms Generation.size
    let quotes = Quote.makeQuotes (map words mylines)
    printlines (Generation.evaluate seeds quotes)
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
