import IO
import Problem022

-- MAIN
main :: IO ()
main = do
    inh <- IO.openFile "Problem022_names.txt" IO.ReadMode
    mylines <- readloop inh []
    IO.hClose inh
    putStrLn (show (answer mylines))

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
