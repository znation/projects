import Problem022
import qualified IO

-- MAIN
main :: IO ()
main = do
    input <- getInput fileName
    putStrLn (show (answer input))

getInput :: String -> IO [String]
getInput fname = do
    inh <- IO.openFile fname IO.ReadMode
    mylines <- readloop inh []
    IO.hClose inh
    return mylines
    
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
