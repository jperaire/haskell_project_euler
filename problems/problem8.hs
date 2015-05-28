import System.IO
import Data.Char

findProducts :: Integral a => a -> [a] -> [a]
findProducts numReps singleList = foldr zipFunction (repeat 1) lists
    where 
        zipFunction = zipWith (*)
        lists = take (fromIntegral numReps) (iterate tail singleList)

largestProductInASeries :: Integral a => a -> [a] -> a
largestProductInASeries numReps numList = maximum . findProducts numReps $ numList

main :: IO ()
main = do
    contents <- readFile "problem8.txt"
    let ints = map digitToInt contents
        res = largestProductInASeries 13 ints 
    print res