--
-- EPITECH PROJECT, 2021
-- B3 - Func. Prog. Seminar
-- File description:
-- Operation
--

rab :: [Int] -> [Int]
rab [] = []
rab [x] = [x]
rab (x:xs) = xs ++ [x]

rr :: [Int] -> [Int] -> ([Int], [Int])
rr (x:xs) (y:ys) = (rab (x:xs), rab (y:ys))

removeLastElem :: [Int] -> [Int]
removeLastElem [] = []
removeLastElem [x] = []
removeLastElem (x:xs) = x : removeLastElem xs

rrab :: [Int] -> [Int]
rrab [] = []
rrab [x] = [x]
rrab (x:xs) = last (x:xs) : removeLastElem (x:xs)

rrr :: [Int] -> [Int] -> ([Int], [Int])
rrr (x:xs) (y:ys) = (rrab (x:xs), rrab (y:ys))