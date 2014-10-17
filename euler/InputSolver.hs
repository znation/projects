import Problem067

import qualified System.IO as IO

-- MAIN
main :: IO ()
main = do
    input <- getInput fileName
    putStrLn (show (answer input))

getInput :: String -> IO [String]
getInput fname = do
    inh <- IO.openFile fname IO.ReadMode
    mylines <- readloop inh []
    let mylines' = reverse mylines
    IO.hClose inh
    
    -- Transform the first line like "abcde","fgh" into ["abcde","fgh"]
    -- let firstLine = head mylines
    -- let tokenStr = ('[':firstLine) ++ "]"
    -- let tokens = (read tokenStr) :: [String]
    -- return tokens
    
    -- Return the list of input lines directly
    return mylines'

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
