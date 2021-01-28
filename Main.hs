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

isValidoperation :: String -> Maybe String
isValidoperation "sa" = Just "sa"
isValidoperation "sb" = Just "sb"
isValidoperation "sc" = Just "sc"
isValidoperation "pa" = Just "pa"
isValidoperation "pb" = Just "pb"
isValidoperation "ra" = Just "ra"
isValidoperation "rb" = Just "rb"
isValidoperation "rr" = Just "rr"
isValidoperation "rra" = Just "rra"
isValidoperation "rrb" = Just "rrb"
isValidoperation "rrr" = Just "rrr"
isValidoperation x = Nothing

checkOperation :: [String] -> Maybe [String]
checkOperation [] = Nothing
checkOperation [x]
        | isValidoperation x == Nothing = Nothing
        | otherwise = Just [fromJust (isValidoperation x)]
checkOperation (x:xs)
        | isValidoperation x == Nothing = Nothing
        | checkOperation xs == Nothing = Nothing
        | otherwise = Just (fromJust (isValidoperation x) : fromJust (checkOperation xs))

main = do
        args <- getArgs
        print args