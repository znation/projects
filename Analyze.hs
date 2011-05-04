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
 
mutateSeed :: [Int] -> TradeState -> TradeState
mutateSeed randomInts s =
        let (d,i) = seedToLists (seed s)
            (randomInt1:randomInt2:_) = randomInts
            idx = randomInt1 `mod` 7 -- idx'th element of seed tuple
        in  if (idx < 4) -- idx 0-3 are Double, 4-6 are Int
            then
                let (xd, yd) = splitAt idx d
                    (y, yd') = splitAt 1 yd
                    y' = ((head y) - 0.20) + ((fromIntegral(randomInt2 `mod` 40))/100.0)
                in  TradeState (cash s) (shares s) (pricePaid s) (listsToSeed (xd ++ (y':yd')) i)
            else
                let (xi, yi) = splitAt (idx-4) i
                    (y, yi') = splitAt 1 yi
                    y' = max (((head y) - 20) + (randomInt2 `mod` 40)) 0
                in  TradeState (cash s) (shares s) (pricePaid s) (listsToSeed d (xi ++ (y':yi')))
         
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
 
mywords :: [String] -> [[String]]
mywords (x:xs) = (words x):(mywords xs)
mywords [] = [[]]
 
makeQuotes :: [[String]] -> [Quote]
makeQuotes (x:xs) = (makeQuote x):(makeQuotes xs)
makeQuotes [] = []
 
makeQuote :: [String] -> Quote
makeQuote (_:y:_) = Quote (read y::Double)
makeQuote _ = Quote 0.0
 
splitQuotesIntoDays :: [Quote] -> [Day]
splitQuotesIntoDays (xs) = if (length xs) < 720
                             then [Day xs]
                             else (Day (take 720 xs)):(splitQuotesIntoDays (drop 360 xs))
 
data Seed = Seed{adjDown, adjStagnant, adjUp, adjPaid :: Double,
        adjDownMin, adjStagnantMin, adjUpMin :: Int}
            deriving (Show, Eq)
instance QC.Arbitrary Seed where
    arbitrary = do
        a <- QC.arbitrary
        b <- QC.arbitrary
        c <- QC.arbitrary
        d <- QC.arbitrary
        e <- QC.arbitrary
        f <- QC.arbitrary
        g <- QC.arbitrary
        return (Seed a b c d (abs e) (abs f) (abs g))
 
-- listsToSeed takes a list of Doubles and a list of Ints and makes it into a Seed
listsToSeed :: [Double] -> [Int] -> Seed
listsToSeed (a:b:c:d:_) (e:f:g:_) = Seed a b c d e f g
listsToSeed _ _ = error "Don't know how to make a seed out of random crap."
 
seedToLists :: Seed -> ([Double], [Int])
seedToLists s =
        let a = adjDown s
            b = adjStagnant s
            c = adjUp s
            d = adjPaid s
            e = adjDownMin s
            f = adjStagnantMin s
            g = adjUpMin s
        in  ((a:b:c:d:[]),(e:f:g:[]))
         
randomSeed :: [Int] -> Seed
randomSeed (r1:r2:r3:r4:r5:r6:r7:_) = 
        let stagmin = r6 `mod` 5
        in  Seed
                    ((fromIntegral(r1 `mod` 10))/100)
                    ((fromIntegral(r2 `mod` 5))/100)
                    ((fromIntegral(r3 `mod` 10))/100)
                    ((fromIntegral(r4 `mod` 5))/100)
                    ((r5 `mod` 5) + stagmin)
                    stagmin
                    ((r7 `mod` 5) + stagmin)
randomSeed _ = error "Can't make a random seed with less than 7 random numbers, idiot!"
                     
randomSeeds :: Int -> [Int] -> [Seed]
randomSeeds 0 _ = []
randomSeeds n randomInts = (randomSeed (take 7 randomInts)):(randomSeeds (n-1) (drop 7 randomInts))
 
data TradeState = TradeState {cash :: Double,
                shares :: Int,
                pricePaid :: Double,
                seed :: Seed}
                deriving (Eq)
instance Ord TradeState where
    compare x y = compare (total x) (total y)
instance Show TradeState where
    show x = "Total: " ++ (show (total x)) ++ "\n" ++ (show (seed x)) ++ "\n"
instance QC.Arbitrary TradeState where
    arbitrary = do
                    a <- QC.arbitrary
                    b <- QC.arbitrary
                    c <- QC.arbitrary
                    d <- QC.arbitrary
                    return (TradeState a b c d)
     
trade :: [Quote] -> Int -> TradeState -> TradeState
trade qs idx state = trade' qs idx state (length qs)
 
trade' :: [Quote] -> Int -> TradeState -> Int -> TradeState
trade' qs idx state len = 
    if  idx >= (len-1)
    then    state
    else    trade' qs (idx+1) (buysell qs idx state) len
 
total :: TradeState -> Double
total state = (cash state) + ((fromIntegral (shares state)) * (pricePaid state))
 
--replaceSeedInState :: (TradeState, Seed) -> TradeState
--replaceSeedInState (t, s) = TradeState (cash t) (shares t) (pricePaid t) s
 
buysell :: [Quote] -> Int -> TradeState -> TradeState
buysell qs idx state = 
        let (buystate, bought) = (buy qs idx state)
        in  if  bought
            then    buystate
            else    let (sellstate, sold) = (sell qs idx state)
                in  if  sold
                    then    sellstate
                    else    state
 
buy :: [Quote] -> Int -> TradeState -> (TradeState, Bool)
buy qs idx state =
    let
        adjDown' = adjDown (seed state)
        adjDownMin' = assert (adjDownMin (seed state) >= 0) (adjDownMin (seed state))
        adjStagnant' = adjStagnant (seed state)
        adjStagnantMin' = assert (adjStagnantMin (seed state) >= 0) (adjStagnantMin (seed state))
        price' = assert (idx < length qs) (price (qs!!idx))
        adjDownMinIdx = assert (idx-adjDownMin' < length qs) (idx-adjDownMin')
        adjStagnantMinIdx = assert (idx-adjStagnantMin' < length qs) (idx-adjStagnantMin')
        cash' = cash state
        shares' = shares state
    in  if cash' >= (commission + price') && (shares' == 0)
        then if idx >= adjDownMin'
            then if price' <= ((price (qs!!adjDownMinIdx))-adjDown')
                then if idx >= adjStagnantMin'
                    then if (abs (price' - (price (qs!!adjStagnantMinIdx)))) <= adjStagnant'
                        then let
                            shares'' = (truncate ((cash' - commission)/price') :: Int)
                            pricePaid'' = price'
                            cash'' = cash' - ((pricePaid'' * (fromIntegral shares'')) + commission)
                        in
                            ((TradeState cash'' shares'' pricePaid'' (seed state)), True)
                        else (state, False)
                    else (state, False)
                else (state, False)
            else (state, False)
        else (state, False)
 
sell :: [Quote] -> Int -> TradeState -> (TradeState, Bool)
sell qs idx state =
    let
        adjPaid' = adjPaid (seed state)
        adjUp' = adjUp (seed state)
        adjUpMin' = adjUpMin (seed state)
        adjStagnant' = adjStagnant (seed state)
        adjStagnantMin' = adjStagnantMin (seed state)
        price' = assert (idx < length qs) (price (qs!!idx))
        cash' = cash state
        shares' = shares state
        pricePaid' = pricePaid state
    in
        if (shares' > 0) && (price' >= pricePaid' + adjPaid')
        then if (idx >= adjUpMin')
            then if (price' >= (price (qs!!(idx-adjUpMin'))) + adjUp')
                then if (idx >= adjStagnantMin')
                    then if (abs(price' - (price (qs!!(idx-adjStagnantMin'))))) <= adjStagnant'
                        then let
                            pricePaid'' = 0.0
                            cash'' = cash' + (price' * (fromIntegral shares')) - commission
                            shares'' = 0
                        in
                            ((TradeState cash'' shares'' pricePaid'' (seed state)), True)
                        else (state, False)
                    else (state, False)
                else (state, False)
            else (state, False)
        else (state, False)
