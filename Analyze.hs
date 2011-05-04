--import qualified Random as Rand
import qualified IO
 
-- MAIN
main :: IO ()
main = do
    inh <- IO.openFile "quotes.tsv" IO.ReadMode
    mylines <- readloop inh []
    --r1 <- Rand.getStdGen
    --let randoms = Rand.randoms r1 :: [Int]
    
    printlines mylines
    
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
 
printlines :: (Show t) => [t] -> IO ()
printlines (x:xs) =
    do
        putStrLn (show x)
        printlines xs
printlines _ = return ()
