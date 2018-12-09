{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Set as S hiding (Set)
import Data.Set (Set)
import Data.List (sort, sortOn, group, nub)
import Data.Char (ord)

cleanInts :: String -> [Int]
cleanInts = map read . lines . filter (/= '+')

numberOne :: IO Int
numberOne = sum . cleanInts <$> readFile "/Users/nwest/1"

dupeFrequency :: Set Int -> Int -> [Int] -> Int
dupeFrequency _ _ [] = 0
dupeFrequency possible accum (x:xs) = let newAccum = accum + x
                                      in if S.member newAccum possible 
                                        then newAccum
                                        else dupeFrequency (S.insert newAccum possible) newAccum xs

numberOneB :: IO Int
numberOneB = dupeFrequency S.empty 0 . cycle . cleanInts <$> readFile "/Users/nwest/1"

-----------------------------------------------

frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

numberTwo :: IO Int
numberTwo =  product . frequency . filter (/= 1) . concatMap (nub . frequency) . lines <$> readFile "/Users/nwest/2"

normalize :: [Int] -> [Int]
normalize = map (\x -> if x == 0 then 0 else 1)

hamming :: String -> String -> Int
hamming s1 = let f = map ord in sum . normalize . zipWith (-) (f s1) . f

combinations :: [a] -> [(a, a)]
combinations xs = concat [ zip (repeat b) . drop a $ xs | (a, b) <- zip [1..] xs ]

third :: (a, b, c) -> c
third (_, _, c) = c

numberTwoB :: IO String
numberTwoB = commonLetters . head . sortOn third . map distance . combinations . lines <$> readFile "/Users/nwest/2"
               where distance set@(l, r) = (l, r, uncurry hamming set)
                     commonLetters (s1, s2, _) = concat $ zipWith (\a b -> if a == b then [a] else "") s1 s2

-----------------------------------------------

data Coordinate = Coordinate Int Int deriving (Eq, Show, Ord)
data Plan = Plan Int Int Int Int deriving (Eq, Show)

splitOn :: Char -> String -> [String]
splitOn c s | not (c `elem` s) = [s]
            | otherwise = let f g = g (/= c) s
                          in [f takeWhile, tail . f $ dropWhile]

parsePlan :: String -> Plan
parsePlan s = let [init -> a, b] = drop 2 . words $ s 
                  [x, y] = f ',' a
                  [w, h] = f 'x' b
              in Plan x y w h
  where
    f x = map read . splitOn x

coordinateMap :: Plan -> [Coordinate]
coordinateMap (Plan x y w h) = [ Coordinate xc yc | xc <- [x..(x + w - 1)], yc <- [y..(y + h - 1)]]

numberThree :: IO Int
numberThree = length . filter ((/=) 1 . length) . group . sort . concatMap (coordinateMap . parsePlan)  . lines <$> readFile "/Users/nwest/3"

-----------------------------------------------

