--
-- EPITECH PROJECT, 2021
-- B3 - Func. Prog. Seminar
-- File description:
-- Main
--

module Main where
import Operation
import System.Environment
import System.Exit
import Data.Maybe
import Data.List

containOnlyInt :: [Char] -> Bool
containOnlyInt [] = False
containOnlyInt [x]
        | x >= '0' && x <= '9' = True
        | otherwise = False
containOnlyInt (x:xs)
        | x >= '0' && x <= '9' = containOnlyInt xs
        | otherwise = False

readInt :: [Char] -> Maybe Int
readInt [] = Nothing
readInt [x]
        | containOnlyInt [x] = Just (read [x]::Int)
        | otherwise = Nothing
readInt (x:xs)
        | containOnlyInt (x:xs) = Just (read (x:xs)::Int)
        | otherwise = Nothing


checkArgs :: [String] -> Maybe [Int]
checkArgs [] = Nothing
checkArgs [x]
        | readInt x == Nothing = Nothing
        | otherwise = Just [fromJust (readInt x)]
checkArgs (x:xs)
        | readInt x == Nothing = Nothing
        | checkArgs xs == Nothing = Nothing
        | otherwise = Just (fromJust (readInt x) : fromJust (checkArgs xs))

isValidOp :: String -> Maybe String
isValidOp "sa" = Just "sa"
isValidOp "sb" = Just "sb"
isValidOp "sc" = Just "sc"
isValidOp "pa" = Just "pa"
isValidOp "pb" = Just "pb"
isValidOp "ra" = Just "ra"
isValidOp "rb" = Just "rb"
isValidOp "rr" = Just "rr"
isValidOp "rra" = Just "rra"
isValidOp "rrb" = Just "rrb"
isValidOp "rrr" = Just "rrr"
isValidOp x = Nothing

checkOp :: [String] -> Maybe [String]
checkOp [] = Nothing
checkOp [x]
        | isValidOp x == Nothing = Nothing
        | otherwise = Just [fromJust (isValidOp x)]
checkOp (x:xs)
        | isValidOp x == Nothing = Nothing
        | checkOp xs == Nothing = Nothing
        | otherwise = Just (fromJust (isValidOp x) : fromJust (checkOp xs))


splitStringOnSpace :: String -> [String]
splitStringOnSpace x
        | (elemIndex ' ' x) == Nothing = [x]
        | otherwise = fst (splitAt (fromJust (elemIndex ' ' x)) x) : splitStringOnSpace (tail (snd (splitAt (fromJust (elemIndex ' ' x)) x)))

haveToReturn :: Maybe [String] -> Maybe [Int] -> IO Int
haveToReturn Nothing y = exitWith (ExitFailure 84)
haveToReturn x Nothing = exitWith (ExitFailure 84)
haveToReturn x y = return 1

checkIfSorted :: (Ord str) => [str] -> Bool
checkIfSorted [] = True
checkIfSorted [x] = True
checkIfSorted (x:y:xs) = x <= y && checkIfSorted (y:xs)

checkPushSwap :: ([Int], [Int]) -> Bool
checkPushSwap (x, []) = checkIfSorted x
checkPushSwap x = False

applyOpEnd  :: [String] -> [Int] -> [Int] -> ([Int], [Int])
applyOpEnd ("pa":xs) y z = applyOp xs (snd (pab z y)) (fst (pab z y))
applyOpEnd ("pb":xs) y z = applyOp xs (fst (pab y z)) (snd (pab y z))
applyOpEnd ("ra":xs) y z = applyOp xs (rab y) z
applyOpEnd ("rb":xs) y z = applyOp xs y (rab z)
applyOpEnd ("rr":xs) y z = applyOp xs (fst (rr y z)) (snd (rr y z))
applyOpEnd ("rra":xs) y z = applyOp xs (rrab y) z
applyOpEnd ("rrb":xs) y z = applyOp xs y (rrab z)
applyOpEnd ("rrr":xs) y z = applyOp xs (fst (rrr y z)) (snd (rrr y z))

applyOpMid :: [String] -> [Int] -> [Int] -> ([Int], [Int])
applyOpMid ["rr"] y z = rr y z
applyOpMid ["rra"] y z = (rrab y, z)
applyOpMid ["rrb"] y z = (y, rrab z)
applyOpMid ["rrr"] y z = rrr z y
applyOpMid ("sa":xs) y z = applyOp xs (sab y) z
applyOpMid ("sb":xs) y z = applyOp xs y (sab z)
applyOpMid ("sc":xs) y z = applyOp xs (fst (sc y z)) (snd (sc y z))
applyOpMid x y z = applyOpEnd x y z

applyOp :: [String] -> [Int] -> [Int] -> ([Int], [Int])
applyOp ["sa"] y z = (sab y, z)
applyOp ["sb"] y z = (y, sab z)
applyOp ["sc"] y z = sc z y
applyOp ["pa"] y z = (snd (pab z y), fst (pab z y))
applyOp ["pb"] y z = pab y z
applyOp ["ra"] y z = (rab y, z)
applyOp ["rb"] y z = (y, rab z)
applyOp x y z = applyOpMid x y z


main = do
        x <- getLine
        y <- getArgs
        haveToReturn (checkOp (splitStringOnSpace x)) (checkArgs y)
        putStr (case checkPushSwap (applyOp (fromJust (checkOp (splitStringOnSpace x))) (fromJust (checkArgs y)) []) of
                True -> "OK\n"
                False -> "KO\n")