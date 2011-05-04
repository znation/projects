-- CONSTANTS
commission :: Double
commission = 8.00
 
-- MAIN
main :: IO ()
main = do
    inh <- openFile "quotes.txt" ReadMode
    mylines <- readloop inh []
    r1 <- getStdGen
    let randomInts = randoms r1 :: [Int]
 
    let qs = makeQuotes (init (mywords (mylines)))
    let numseeds = 40
    let days = splitQuotesIntoDays(qs)
    let seeds = randomSeeds numseeds randomInts
    let states = map (TradeState 1000.0 0 0) seeds
     
    printlines (generation 0 states days (drop (7*numseeds) randomInts))
 
    hClose inh
 
generation :: Int -> [TradeState] -> [Day] -> [Int] -> [TradeState]
generation 50 states days _ = best 20 states (quotes(head days))
generation n states days randomInts =
        let newstates = generate (best 20 states (quotes (head days))) (take 80 randomInts)
        in  if  (n `mod` 100 == 99)
                -- advance to the next day's quotes every 100 generations, at 99, 199, 299, etc
                then    generation (n+1) newstates (drop 1 days) (drop 80 randomInts)
                else    generation (n+1) newstates days (drop 80 randomInts)
 
best :: Int -> [TradeState] -> [Quote] -> [TradeState]
best n states qs =
        let results = assert (length (map (trade qs 0) states) >= n) (map (trade qs 0) states)
        in  take n (reverse (sort results))
     
generate :: [TradeState] -> [Int] -> [TradeState]
generate (state:states) randomInts = 
        let a = mutateSeed (take 2 randomInts) state
            b = mutateSeed (take 2 randomInts) state
            rest = generate states (drop 4 randomInts)
        in  a:b:rest
generate [] _ = []
 
readloop :: Handle -> [String] -> IO [String]
readloop inh array = 
    do
        ineof <- hIsEOF inh
        if ineof
            then return array
            else
				do
					inpStr <- hGetLine inh
					readloop inh (inpStr:array)
 
printlines :: (Show t) => [t] -> IO ()
printlines (x:xs) =
    do
        putStrLn (show x)
        printlines xs
printlines _ = return ()
