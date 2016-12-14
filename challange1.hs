import Data.Char (digitToInt)


challange = ["R5", "L2", "L1", "R1", "R3", "R3", "L3", "R3", "R4", "L2", "R4", "L4", "R4", "R3", "L2", "L1", "L1", "R2", "R4", "R4", "L4", "R3", "L2", "R1", "L4", "R1", "R3", "L5", "L4", "L5", "R3", "L3", "L1", "L1", "R4", "R2", "R2", "L1", "L4", "R191", "R5", "L2", "R46", "R3", "L1", "R74", "L2", "R2", "R187", "R3", "R4", "R1", "L4", "L4", "L2", "R4", "L5", "R4", "R3", "L2", "L1", "R3", "R3", "R3", "R1", "R1", "L4", "R4", "R1", "R5", "R2", "R1", "R3", "L4", "L2", "L2", "R1", "L3", "R1", "R3", "L5", "L3", "R5", "R3", "R4", "L1", "R3", "R2", "R1", "R2", "L4", "L1", "L1", "R3", "L3", "R4", "L2", "L4", "L5", "L5", "L4", "R2", "R5", "L4", "R4", "L2", "R3", "L4", "L3", "L5", "R5", "L4", "L2", "R3", "R5", "R5", "L1", "L4", "R3", "L1", "R2", "L5", "L1", "R4", "L1", "R5", "R1", "L4", "L4", "L4", "R4", "R3", "L5", "R1", "L3", "R4", "R3", "L2", "L1", "R1", "R2", "R2", "R2", "L1", "L1", "L2", "L5", "L3", "L1"]

solve = calcDistance challange
{-
  the tuple is (rotation, x-distance, y-distance)

  where the rotation is, 0 = north, 1 = east, 2 = south, 3 = west
-}
calcDistance :: [[Char]] -> Int
calcDistance [] = 0
calcDistance path = manhattanDistance $ foldl walk (0,0,0) path

manhattanDistance :: (Int, Int, Int) -> Int
manhattanDistance (_, a, b) = abs a + abs b

walk :: (Int, Int, Int) -> [Char] -> (Int, Int, Int)
walk (0, x, y) ('R':xs) = (1, x + parseInt xs, y)
walk (1, x, y) ('R':xs) = (2, x, y - parseInt xs)
walk (2, x, y) ('R':xs) = (3, x - parseInt xs, y)
walk (3, x, y) ('R':xs) = (0, x, y + parseInt xs)
walk (0, x, y) ('L':xs) = (3, x - parseInt xs, y)
walk (1, x, y) ('L':xs) = (0, x, y + parseInt xs)
walk (2, x, y) ('L':xs) = (1, x + parseInt xs, y)
walk (3, x, y) ('L':xs) = (2, x, y - parseInt xs)
walk a _ = a

parseInt :: [Char] -> Int
parseInt [] = 0
parseInt (a:[]) = digitToInt a
parseInt (a:xs) = (digitToInt a) * (10 ^ length xs) + parseInt xs
