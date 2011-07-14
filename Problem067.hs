module Problem067 where

import qualified Data.MemoCombinators as Memo
import Utility

fileName :: String
fileName = "Problem067_triangle.txt"

answer :: [String] -> Integer
answer input =  let tree = buildTree input

                    sumAt :: Int -> Int -> Integer
                    sumAt = Memo.memo2 Memo.integral Memo.integral sumAt'

                    sumAt' :: Int -> Int -> Integer
                    sumAt' 0 colIdx = (tree !! 0) !! colIdx
                    sumAt' rowIdx colIdx =  let row = tree !! rowIdx
                                                value = row !! colIdx `debug` ("Value at " ++ (show (rowIdx,colIdx)))
                                                prevRowIdx = rowIdx - 1
                                                leftColIdx = colIdx - 1
                                                rightColIdx = colIdx
                                                lastCol = (length row) - 1 == colIdx
                                                firstCol = 0 == colIdx
                                                leftValue = sumAt prevRowIdx leftColIdx
                                                rightValue = sumAt prevRowIdx rightColIdx
                                            in  if      lastCol
                                                then    value + leftValue
                                                else    if      firstCol
                                                        then    value + rightValue
                                                        else    value + (max leftValue rightValue)

                    walkTree :: Integer
                    walkTree =  let lastRowIdx = (length tree) - 1
                                    lastRow = tree !! lastRowIdx
                                    lastColIdx = (length lastRow) - 1
                                in  maximum (map (uncurry sumAt) (handshake [lastRowIdx] [0..lastColIdx]))
                                
                in  walkTree

-- Transform the tree structure like "a b\nc d e\n" into [[a,b], [c,d,e]]
buildTree :: [String] -> [[Integer]]
buildTree [] = []
buildTree (x:xs) =  let rowStr :: [String]
                        rowStr = split x ' '
                        rowInt :: [Integer]
                        rowInt = map read rowStr
                    in  rowInt:(buildTree xs)

                

