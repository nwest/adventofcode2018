{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Set as S hiding (Set)
import Data.Set (Set)
import Data.List (sort, sortOn, group, nub)
import Data.Char (ord)
import Data.Tuple

cleanInts :: String -> [Int]
cleanInts = map read . lines . filter (/= '+')

numberOne :: IO Int
numberOne = sum . cleanInts <$> readFile "/Users/nwest/AoC/1"

dupeFrequency :: Set Int -> Int -> [Int] -> Int
dupeFrequency _ _ [] = 0
dupeFrequency possible accum (x:xs) = let newAccum = accum + x
                                      in if S.member newAccum possible 
                                        then newAccum
                                        else dupeFrequency (S.insert newAccum possible) newAccum xs

numberOneB :: IO Int
numberOneB = dupeFrequency S.empty 0 . cycle . cleanInts <$> readFile "/Users/nwest/AoC/1"

-----------------------------------------------

frequency :: Ord a => [a] -> [Int]
frequency = map length . group . sort

numberTwo :: IO Int
numberTwo =  product . frequency . filter (/= 1) . concatMap (nub . frequency) . lines <$> readFile "/Users/nwest/AoC/2"

normalize :: [Int] -> [Int]
normalize = map (\x -> if x == 0 then 0 else 1)

hamming :: String -> String -> Int
hamming s1 = let f = map ord in sum . normalize . zipWith (-) (f s1) . f

combinations :: [a] -> [(a, a)]
combinations xs = concat [ zip (repeat b) . drop a $ xs | (a, b) <- zip [1..] xs ]

third :: (a, b, c) -> c
third (_, _, c) = c

numberTwoB :: IO String
numberTwoB = commonLetters . head . sortOn third . map distance . combinations . lines <$> readFile "/Users/nwest/AoC/2"
               where distance set@(l, r) = (l, r, uncurry hamming set)
                     commonLetters (s1, s2, _) = concat $ zipWith (\a b -> if a == b then [a] else "") s1 s2

-----------------------------------------------

type Id = Int
type XCoord = Int
type YCoord = Int
type Width = Int
type Height = Int

data Coordinate = Coordinate XCoord YCoord deriving (Eq, Show, Ord)
data Plan = Plan Id XCoord YCoord Width Height deriving (Eq, Show)

splitOn :: Char -> String -> [String]
splitOn c s | c `notElem` s = [s]
            | otherwise = let f g = g (/= c) s
                          in [f takeWhile, tail . f $ dropWhile]

-- #1319 @ 627,639: 23x11
parsePlan :: String -> Plan
parsePlan s = let [(read . tail) -> num, _, init -> cs, wh] = words s
                  [x, y] = f ',' cs
                  [w, h] = f 'x' wh
              in Plan num x y w h
  where
    f x = map read . splitOn x

coordinateMap :: Plan -> [Coordinate]
coordinateMap (Plan _ x y w h) = [ Coordinate xc yc | xc <- [x..(x + w - 1)], yc <- [y..(y + h - 1)] ]

numberThree :: IO Int
numberThree = length . filter ((/=) 1 . length) . group . sort . concatMap (coordinateMap . parsePlan)  . lines <$> readFile "/Users/nwest/AoC/3"

checkOverlaps :: Ord a => [Set a] -> [Bool]
checkOverlaps set = map (\d -> overlapsWith d (filter (/= d) set)) set
                    where
                      overlapsWith a = any (not . S.disjoint a)

numberThreeB :: IO Plan
numberThreeB = let plans = map parsePlan . lines <$> readFile "/Users/nwest/AoC/3"
                   overlaps = checkOverlaps . map (S.fromList . coordinateMap) <$> plans
               in fst . head . filter (\(_, o) -> not o) <$> (zip <$> plans <*> overlaps)

-----------------------------------------------

-- data Event = Wake | Sleep | Start DateTime

numberFour :: IO [String]
numberFour = lines <$> readFile "/Users/nwest/AoC/4"

-----------------------------------------------

polymerCombos :: Set (Char, Char)
polymerCombos = let xs = zip ['a'..'z'] ['A'..'Z']
                in S.fromList (xs ++ map swap xs)

byTwos :: Ord a => [a] -> Set (a, a)
byTwos s = S.fromList (zip s . tail $ s)

hasReactions :: String -> Bool
hasReactions s = let split = byTwos s
                 in not (S.disjoint polymerCombos split)

react :: String -> String
react [] = []
react [a] = [a]
react [a, b] = if S.member (a, b) polymerCombos then "" else [a, b]
react (a:b:c:xs) | S.member (a, b) polymerCombos = react (c:xs)
                 | S.member (b, c) polymerCombos = react (a:xs)
                 | otherwise = a:b:c:react xs

tryReact :: String -> String
tryReact s | hasReactions s = f
           | hasReactions (tail s) = f
           | otherwise = s
           where f = tryReact (react s)

numberFive :: IO Int
numberFive = length . tryReact . init <$> readFile "/Users/nwest/AoC/5"

-----------------------------------------------

