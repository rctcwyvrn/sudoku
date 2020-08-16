module Sudoku (initialize, solve, printGrid) where

import Data.Bool
import Data.List
import Data.List.Split
import Data.Maybe

data Row =
  Row
    { val :: [Int]
    }
  deriving (Show)

data Grid =
  Grid
    { rows :: [Row]
    }
  deriving (Show)

full :: Grid -> Bool
full g = all (/= 0) (foldl (\acc x -> acc ++ (val x)) [] (rows g))

transposeRow :: [Row] -> [[Int]]
transposeRow rs = transpose (map val rs)

sketch :: [Row] -> Int -> [Int]
sketch rs i = foldl (\acc r -> acc ++ take 3 (drop (3 * i) (val r))) [] rs

blockify :: [Row] -> [[Int]]
blockify rs = a ++ b ++ c
  where
    base = [0, 1, 2]
    a = map (sketch (take 3 rs)) base
    b = map (sketch (take 3 (drop 3 rs))) base
    c = map (sketch (take 3 (drop 6 rs))) base

validateSectionHelper :: [Int] -> [Int] -> Bool
validateSectionHelper _ [] = True
validateSectionHelper visited (x:xs)
  | x == 0 = validateSectionHelper visited xs
  | x `elem` visited = False
  | otherwise = validateSectionHelper (visited ++ [x]) xs

validateSection :: [Int] -> Bool
validateSection = validateSectionHelper []

-- validate that the grid so far is valid
valid :: Grid -> Bool
valid (Grid rows) = validRows && validColumns && validBlocks
  where
    validRows = all (validateSection) (map val rows)
    validColumns = all (validateSection) (transposeRow rows)
    validBlocks = all (validateSection) (blockify rows)

-- Returns the first blank space as a raw position (0 to 81)
findSpace :: [Int] -> Maybe Int
findSpace = findIndex (\x -> x == 0)

createGrids :: [Int] -> Maybe Int -> [Grid]
createGrids _ Nothing = []
createGrids xs (Just i) = [Grid rows | rows <- asRows]
  where
    (before, after) = splitAt i xs
    rawGrids = [before ++ [n] ++ (tail after) | n <- [1 .. 9]]
    asRows = [[Row xs | xs <- (chunksOf 9 grid)] | grid <- rawGrids]

getNextGrids :: Grid -> [Grid]
getNextGrids (Grid rows) =
  filter (\g -> valid g) (createGrids everything (findSpace everything))
  where
    everything = foldl (\acc row -> acc ++ (val row)) [] rows

solveListOfGrid :: [Grid] -> Maybe Grid
solveListOfGrid [] = Nothing
solveListOfGrid gx
  | isNothing res = solveListOfGrid (tail gx)
  | otherwise = res
  where
    res = solve (head gx)

-- Recursively attempt to solve the grid, return Nothing if grid can't be solved 
solve :: Grid -> Maybe Grid
solve g
  | full g = Just g
  | otherwise = solveListOfGrid (getNextGrids g)

parseGrid :: String -> [[Int]]
parseGrid s = [map (\x -> read x) [[x] | x <- row] | row <- rows]
  where
    rows = splitOn "\n" s

initialize :: String -> Grid
initialize s = Grid rows
  where
    grid = parseGrid s
    rows = [Row xs | xs <- grid]

printRow :: Row -> String
printRow (Row vals) =
  foldl (\acc val -> acc ++ "," ++ (show val)) (show (head vals)) (tail vals)

printGrid :: Maybe Grid -> String
printGrid Nothing = "Solve failed"
printGrid (Just (Grid rows)) =
  foldl
    (\acc row -> acc ++ "\n" ++ printRow row)
    (printRow (head rows))
    (tail rows)

-- Usage: cat grid | ./sudoku
main :: IO ()
main = do
  grid <- getContents
  putStrLn $ printGrid $ solve $ initialize grid
