--
-- EPITECH PROJECT, 2021
-- B3 - Func. Prog. Seminar
-- File description:
-- Op
--

import System.Environment
import Data.Maybe

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

main = do
        args <- checkArgs getArgs
        print args